"""Download all chart data from the MedBench Platform and save as allData.json.

This file is required by build_processed_output.R (extend_with_platform_JSON).

Usage:
    python download_platform_charts.py --url http://localhost:4000 --email admin@example.com
    python download_platform_charts.py --url http://localhost:4000 --token <jwt_token>
    python download_platform_charts.py  # reads MEDBENCH_URL, MEDBENCH_TOKEN or
    # MEDBENCH_EMAIL/MEDBENCH_PASSWORD from env
"""

import argparse
import json
from pathlib import Path

from python_helpers.platform_graphql import (
    add_platform_auth_args,
    graphql_request,
    resolve_token,
)

CHARTS4EXPORT_QUERY = """
query ExportCharts($allVersions: Boolean) {
  charts4export(allVersions: $allVersions) {
    id
    name
    specialty
    charts {
      id
      caseId
      name
      specialty
      language
      subLanguage
      isOriginal
      version
      status
      notes {
        id
        type
        date
        time
        author
        content
        chartTranslationId
      }
      medications {
        id
        chartTranslationId
        atcCode
        medication
        wayOfAdministration
        strength
        unit
        timesPerDay
        date
      }
      labValues {
        id
        chartTranslationId
        labTest
        value
        unit
        referenceInterval
        date
        time
      }
      summaries {
        id
        chartTranslationId
        generatedBy
        generatorType
        text
        status
      }
    }
  }
}
"""

def download_charts(url: str, token: str, all_versions: bool = False) -> list:
    """Download chart export data from the Platform.

    Args:
        url: Full GraphQL endpoint URL.
        token: JWT access token for an admin user.
        all_versions: Whether to include all case versions instead of only the latest.

    Returns:
        List of exported cases in the ``charts4export`` GraphQL shape.
    """
    data = graphql_request(
        url,
        CHARTS4EXPORT_QUERY,
        {"allVersions": all_versions},
        token=token,
    )
    return data["charts4export"]


def main() -> None:
    """Parse CLI arguments, download chart data, and write the JSON export."""
    parser = argparse.ArgumentParser(
        description="Download chart data from the MedBench Platform"
    )
    add_platform_auth_args(parser)
    parser.add_argument("--out", default="data/output/allData.json")
    parser.add_argument(
        "--all-versions",
        action="store_true",
        help="Include all case versions (default: latest only)",
    )
    args = parser.parse_args()

    token = resolve_token(args, parser)

    print(f"Downloading charts from {args.url}...")
    charts = download_charts(args.url, token, all_versions=args.all_versions)
    print(
        f"Downloaded {len(charts)} cases ({sum(len(c.get('charts', [])) for c in charts)} chart translations)."
    )

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(charts, f, ensure_ascii=False, indent=2)
    print(f"Saved to {out_path}")


if __name__ == "__main__":
    main()
