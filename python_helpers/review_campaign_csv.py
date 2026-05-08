"""CSV output helpers for flattened review campaign rows."""

import csv
from pathlib import Path
from typing import Any

from python_helpers.pdqi import COMMENT_FIELDS, PDQI_DOMAINS


def write_csv(rows: list[dict[str, Any]], out_path: Path) -> None:
    """Write flattened review rows to CSV using a stable column order."""
    out_path.parent.mkdir(parents=True, exist_ok=True)
    fieldnames = [
        "campaignId",
        "campaignLabel",
        "campaignReviewType",
        "reviewId",
        "reviewCreatedAt",
        "reviewType",
        "seconds2review",
        "reviewerId",
        "reviewerName",
        "reviewerEmail",
        "chartTranslationId",
        "summaryId",
        "caseId",
        "caseName",
        "specialty",
        "language",
        "subLanguage",
        "summaryGeneratedBy",
        "summaryGeneratorType",
        "promptType",
        "summaryStatus",
        "summaryCreatedAt",
        "summaryCreatedBy",
        "summaryTextLength",
        "summaryWordCount",
        "chartTokenCount",
        "chartWordCount",
        "noteCount",
        "medicationCount",
        "labValueCount",
        "factCount",
        "lengthOfStayDays",
        *[f"pdqi_{domain}" for domain in PDQI_DOMAINS],
        "pdqi9_total",
        "pdqi9_mean",
        "pdqi9_complete",
        "safety",
        "bias",
        "hallucinationSeverity",
        "overall",
        "blinding",
        "version",
        "partial",
        *COMMENT_FIELDS,
        *[f"pdqi_{domain}__comment" for domain in PDQI_DOMAINS],
        "experienceSnapshot",
    ]
    extra_fields = sorted({key for row in rows for key in row} - set(fieldnames))
    fieldnames.extend(extra_fields)

    with open(out_path, "w", encoding="utf-8", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)
