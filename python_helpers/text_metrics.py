"""Text serialization and length metrics for chart exports."""

import re
from datetime import date
from typing import Any


def word_count(text: str) -> int:
    """Count word-like tokens in text for descriptive chart and summary lengths."""
    return len(re.findall(r"\b\w+\b", text, flags=re.UNICODE))


def token_count(text: str) -> int:
    """Approximate LLM token count using words and standalone punctuation.

    This is deliberately dependency-free and reproducible; it is not intended to
    match any vendor tokenizer exactly.
    """
    return len(re.findall(r"\w+|[^\w\s]", text, flags=re.UNICODE))


def medication_to_text(medication: dict[str, Any]) -> str:
    """Serialize one medication row into text for chart-length calculation."""
    values = [
        medication.get("date"),
        medication.get("medication"),
        medication.get("atcCode"),
        medication.get("wayOfAdministration"),
        medication.get("strength"),
        medication.get("unit"),
        medication.get("timesPerDay"),
    ]
    return " ".join(str(value) for value in values if value not in (None, ""))


def lab_value_to_text(lab_value: dict[str, Any]) -> str:
    """Serialize one lab-value row into text for chart-length calculation."""
    values = [
        lab_value.get("date"),
        lab_value.get("time"),
        lab_value.get("labTest"),
        lab_value.get("value"),
        lab_value.get("unit"),
        lab_value.get("referenceInterval"),
    ]
    return " ".join(str(value) for value in values if value not in (None, ""))


def fact_to_text(fact: dict[str, Any]) -> str:
    """Serialize one case fact into text for chart-length calculation."""
    values = [fact.get("question"), fact.get("answer")]
    return " ".join(str(value) for value in values if value not in (None, ""))


def chart_source_text(chart: dict[str, Any], facts: list[dict[str, Any]]) -> str:
    """Build the source-text representation used for chart length metrics.

    The text includes note headers/content, medication rows, lab rows, and case
    facts. It is an analysis representation only; the raw structured fields are
    still preserved in the JSON export.
    """
    parts: list[str] = []
    for note in chart.get("notes", []):
        header = " ".join(
            str(value)
            for value in [
                note.get("date"),
                note.get("time"),
                note.get("type"),
                note.get("author"),
            ]
            if value not in (None, "")
        )
        content = note.get("content") or ""
        parts.append(f"{header}\n{content}".strip())
    parts.extend(medication_to_text(med) for med in chart.get("medications", []))
    parts.extend(lab_value_to_text(lab) for lab in chart.get("labValues", []))
    parts.extend(fact_to_text(fact) for fact in facts)
    return "\n\n".join(part for part in parts if part)


def length_of_stay_days(chart: dict[str, Any]) -> int | None:
    """Estimate length of stay as max minus min dated chart element.

    Dates are read from notes, medications, and lab values. Invalid or missing
    dates are skipped; ``None`` is returned when no valid date is available.
    """
    dates = []
    for item in [
        *chart.get("notes", []),
        *chart.get("medications", []),
        *chart.get("labValues", []),
    ]:
        value = item.get("date")
        if not value:
            continue
        try:
            dates.append(date.fromisoformat(value))
        except ValueError:
            continue
    if not dates:
        return None
    return (max(dates) - min(dates)).days
