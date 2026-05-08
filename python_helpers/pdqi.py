"""PDQI+ rating normalization helpers."""

from typing import Any

PDQI_DOMAINS = [
    "accurate",
    "thorough",
    "useful",
    "clear",
    "organized",
    "succinct",
    "synthesizes",
    "internallyConsistent",
    "upToDate",
]

COMMENT_FIELDS = [
    "accurate__comment",
    "thorough__comment",
    "useful__comment",
    "internallyConsistent__comment",
    "upToDate__comment",
    "safety__comment",
    "bias__comment",
    "hallucinationSeverity__comment",
    "overallComment",
]


def safe_int(value: Any) -> int | None:
    """Return ``value`` as an integer, or ``None`` when conversion is not possible."""
    if value is None or value == "":
        return None
    try:
        return int(value)
    except (TypeError, ValueError):
        return None


def normalize_rating(rating: dict[str, Any]) -> dict[str, Any]:
    """Flatten a PDQI+ rating JSON object into analysis columns.

    The returned mapping includes one column per PDQI-9 domain, optional domain
    comments, PDQI-9 total/mean/completeness, and the additional PDQI+ fields
    such as safety, bias, hallucination severity, overall, and blinding.
    """
    pdqi = rating.get("pdqi") or {}
    row: dict[str, Any] = {}
    for domain in PDQI_DOMAINS:
        row[f"pdqi_{domain}"] = pdqi.get(domain)
        comment = pdqi.get(f"{domain}__comment")
        if comment is not None:
            row[f"pdqi_{domain}__comment"] = comment

    pdqi_values = [safe_int(row.get(f"pdqi_{domain}")) for domain in PDQI_DOMAINS]
    numeric_pdqi = [value for value in pdqi_values if value is not None]
    row["pdqi9_total"] = sum(numeric_pdqi) if numeric_pdqi else None
    row["pdqi9_mean"] = (
        round(sum(numeric_pdqi) / len(numeric_pdqi), 4) if numeric_pdqi else None
    )
    row["pdqi9_complete"] = len(numeric_pdqi) == len(PDQI_DOMAINS)

    for field in [
        "safety",
        "bias",
        "hallucinationSeverity",
        "overall",
        "blinding",
        "version",
        "partial",
        "safety__comment",
        "bias__comment",
        "hallucinationSeverity__comment",
        "overallComment",
    ]:
        row[field] = rating.get(field)
    return row
