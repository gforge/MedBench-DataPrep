"""Campaign selection, enrichment, and flattening helpers."""

from __future__ import annotations

import json
import re
from typing import Any

from python_helpers.pdqi import normalize_rating
from python_helpers.text_metrics import (
    chart_source_text,
    length_of_stay_days,
    token_count,
    word_count,
)


def slugify(value: str) -> str:
    """Convert a campaign label into a filesystem-friendly slug."""
    slug = re.sub(r"[^A-Za-z0-9]+", "_", value.strip()).strip("_").lower()
    return slug or "campaign"


def resolve_campaign(
    campaigns: list[dict[str, Any]],
    campaign_label: str | None,
    campaign_id: str | None,
) -> dict[str, Any]:
    """Select exactly one campaign by id or exact label.

    ``campaign_id`` takes precedence. When both id and label are provided, the
    label is treated as a consistency check. Label lookup intentionally requires
    an exact unique match to avoid exporting the wrong campaign.

    Raises:
        ValueError: If no campaign matches, the label is ambiguous, or id and
            label refer to different campaigns.
    """
    if campaign_id:
        matches = [campaign for campaign in campaigns if campaign["id"] == campaign_id]
        if not matches:
            available = "\n".join(
                f"- {campaign['id']}: {campaign['label']}" for campaign in campaigns
            )
            raise ValueError(
                f"No campaign found with id '{campaign_id}'. Available campaigns:\n{available}"
            )
        campaign = matches[0]
        if campaign_label and campaign["label"] != campaign_label:
            raise ValueError(
                "Campaign id and label refer to different campaigns: "
                f"id '{campaign_id}' has label '{campaign['label']}', "
                f"not '{campaign_label}'."
            )
        return campaign

    if not campaign_label:
        available = "\n".join(
            f"- {campaign['id']}: {campaign['label']}" for campaign in campaigns
        )
        raise ValueError(
            "Provide --campaign-label or --campaign-id. Available campaigns:\n"
            f"{available}"
        )

    matches = [campaign for campaign in campaigns if campaign["label"] == campaign_label]
    if len(matches) == 1:
        return matches[0]

    if not matches:
        available = "\n".join(
            f"- {campaign['id']}: {campaign['label']}" for campaign in campaigns
        )
        raise ValueError(
            f"No campaign found with exact label '{campaign_label}'. "
            f"Available campaigns:\n{available}"
        )

    candidates = "\n".join(f"- {campaign['id']}: {campaign['label']}" for campaign in matches)
    raise ValueError(
        f"Found {len(matches)} campaigns with label '{campaign_label}'. "
        f"Use --campaign-id. Matching campaigns:\n{candidates}"
    )


def full_name(user: dict[str, Any] | None) -> str:
    """Return a display name from Platform user fields."""
    if not user:
        return ""
    names = [user.get("firstName"), user.get("middleName"), user.get("lastName")]
    return " ".join(str(name) for name in names if name)


def generator_metadata(summary: dict[str, Any] | None) -> tuple[str, str, str]:
    """Return generatedBy, generatorType, and normalized prompt type for a summary."""
    if not summary:
        return "", "", ""
    generated_by = summary.get("generatedBy") or ""
    generator_type = summary.get("generatorType") or ""
    if generator_type == "HUMAN" or generated_by == "Human":
        return generated_by, generator_type or "HUMAN", "Human"
    if "@basic" in generated_by:
        return generated_by, generator_type or "LLM", "Single"
    if "@decompos" in generated_by:
        return generated_by, generator_type or "LLM", "Multistep"
    prompt_type = generated_by.rsplit("@", 1)[-1] if "@" in generated_by else ""
    return generated_by, generator_type, prompt_type


def enrich_campaign_charts(
    campaign: dict[str, Any], all_cases: list[dict[str, Any]]
) -> dict[str, Any]:
    """Merge campaign chart context with the full chart export.

    The campaign query supplies the review-specific chart/summary subset, while
    ``charts4export`` carries case facts and fuller chart metadata. This function
    merges them by chart translation id and adds chart-length features.
    """
    exported_charts: dict[str, tuple[dict[str, Any], dict[str, Any]]] = {}
    for case in all_cases:
        for chart in case.get("charts", []):
            exported_charts[chart["id"]] = (case, chart)

    enriched_charts = []
    for chart in campaign.get("charts", []):
        case, exported_chart = exported_charts.get(chart["id"], ({}, {}))
        facts = case.get("facts", [])
        enriched = {
            **chart,
            **{key: value for key, value in exported_chart.items() if key not in {"summaries"}},
            "facts": facts,
            "factCount": case.get("factCount", len(facts) if facts else None),
            "factStatus": case.get("factStatus"),
            "caseId": exported_chart.get("caseId", chart.get("caseId")),
            "subLanguage": exported_chart.get("subLanguage", chart.get("subLanguage")),
            "isOriginal": exported_chart.get("isOriginal", chart.get("isOriginal")),
            "version": exported_chart.get("version", chart.get("version")),
            "status": exported_chart.get("status", chart.get("status")),
            "createdAt": exported_chart.get("createdAt", chart.get("createdAt")),
            "createdBy": exported_chart.get("createdBy", chart.get("createdBy")),
            "summaries": merge_summaries(
                chart.get("summaries", []), exported_chart.get("summaries", [])
            ),
        }
        source_text = chart_source_text(enriched, facts)
        enriched["lengthFeatures"] = {
            "chartTokenCount": token_count(source_text),
            "chartWordCount": word_count(source_text),
            "noteCount": len(enriched.get("notes", [])),
            "medicationCount": len(enriched.get("medications", [])),
            "labValueCount": len(enriched.get("labValues", [])),
            "factCount": enriched.get("factCount"),
            "lengthOfStayDays": length_of_stay_days(enriched),
        }
        enriched_charts.append(enriched)

    return {**campaign, "charts": enriched_charts}


def merge_summaries(
    campaign_summaries: list[dict[str, Any]], exported_summaries: list[dict[str, Any]]
) -> list[dict[str, Any]]:
    """Merge campaign summary rows with full exported summary rows by id."""
    exported_by_id = {summary["id"]: summary for summary in exported_summaries}
    merged = []
    seen = set()
    for summary in campaign_summaries:
        exported = exported_by_id.get(summary["id"], {})
        merged.append({**summary, **exported})
        seen.add(summary["id"])
    for summary in exported_summaries:
        if summary["id"] not in seen:
            merged.append(summary)
    return merged


def build_indexes(campaign: dict[str, Any]) -> tuple[dict[str, Any], dict[str, Any]]:
    """Build lookup dictionaries for charts and summaries in a campaign export."""
    charts_by_id = {chart["id"]: chart for chart in campaign.get("charts", [])}
    summaries_by_id = {}
    for chart in campaign.get("charts", []):
        for summary in chart.get("summaries", []):
            summaries_by_id[summary["id"]] = {**summary, "_chart": chart}
    return charts_by_id, summaries_by_id


def flatten_reviews(
    campaign: dict[str, Any], reviews: list[dict[str, Any]]
) -> list[dict[str, Any]]:
    """Convert nested campaign reviews into one analysis row per review.

    Rows combine campaign metadata, chart metadata, summary metadata, reviewer
    identity, chart-length features, and flattened PDQI+ rating fields.
    """
    charts_by_id, summaries_by_id = build_indexes(campaign)
    rows = []

    for review in reviews:
        summary_id = review.get("summaryId")
        chart_translation_id = review.get("chartTranslationId")
        summary = summaries_by_id.get(summary_id) if isinstance(summary_id, str) else None
        chart = (
            charts_by_id.get(chart_translation_id)
            if isinstance(chart_translation_id, str)
            else None
        )
        if summary and summary.get("_chart"):
            chart = summary["_chart"]
        length_features = chart.get("lengthFeatures", {}) if chart else {}
        generated_by, generator_type, prompt_type = generator_metadata(summary)
        rating = normalize_rating(review.get("rating") or {})
        user = review.get("user") or {}
        row = {
            "campaignId": campaign.get("id"),
            "campaignLabel": campaign.get("label"),
            "campaignReviewType": campaign.get("reviewType"),
            "reviewId": review.get("id"),
            "reviewCreatedAt": review.get("createdAt"),
            "reviewType": review.get("reviewType"),
            "seconds2review": review.get("seconds2review"),
            "reviewerId": review.get("userId"),
            "reviewerName": full_name(user),
            "reviewerEmail": user.get("userMainEmail"),
            "chartTranslationId": chart_translation_id,
            "summaryId": summary_id,
            "caseId": chart.get("caseId") if chart else "",
            "caseName": chart.get("name") if chart else "",
            "specialty": chart.get("specialty") if chart else "",
            "language": chart.get("language") if chart else "",
            "subLanguage": chart.get("subLanguage") if chart else "",
            "summaryGeneratedBy": generated_by,
            "summaryGeneratorType": generator_type,
            "promptType": prompt_type,
            "summaryStatus": summary.get("status") if summary else "",
            "summaryCreatedAt": summary.get("createdAt") if summary else "",
            "summaryCreatedBy": summary.get("createdBy") if summary else "",
            "summaryTextLength": len(summary.get("text") or "") if summary else None,
            "summaryWordCount": word_count(summary.get("text") or "") if summary else None,
            "experienceSnapshot": json.dumps(
                review.get("experienceSnapshot"), ensure_ascii=False, sort_keys=True
            )
            if review.get("experienceSnapshot") is not None
            else "",
            **length_features,
            **rating,
        }
        rows.append(row)

    return rows
