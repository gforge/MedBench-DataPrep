"""Download all chart data from the MedBench Platform and save as allData.json.

This file is required by build_processed_output.R (extend_with_platform_JSON).

Usage:
    python download_platform_charts.py --url http://localhost:4000 --email admin@example.com --password secret
    python download_platform_charts.py --url http://localhost:4000 --token <jwt_token>
    python download_platform_charts.py  # reads MEDBENCH_URL, MEDBENCH_TOKEN or MEDBENCH_EMAIL/MEDBENCH_PASSWORD from env
"""

import argparse
import json
import os
import urllib.error
import urllib.request
from pathlib import Path

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

LOGIN_MUTATION = """
mutation Login($email: String!, $password: String!) {
  login(email: $email, password: $password) {
    accessToken
  }
}
"""


def graphql_request(url: str, query: str, variables: dict, token: str | None = None) -> dict:
    payload = json.dumps({"query": query, "variables": variables}).encode()
    headers = {"Content-Type": "application/json"}
    if token:
        headers["Authorization"] = f"Bearer {token}"
    req = urllib.request.Request(url, data=payload, headers=headers, method="POST")
    try:
        with urllib.request.urlopen(req) as resp:
            body = json.loads(resp.read())
    except urllib.error.HTTPError as e:
        body = json.loads(e.read())
    if "errors" in body:
        raise RuntimeError(f"GraphQL error: {body['errors']}")
    return body["data"]


def get_token(url: str, email: str, password: str) -> str:
    data = graphql_request(url, LOGIN_MUTATION, {"email": email, "password": password})
    return data["login"]["accessToken"]


def download_charts(url: str, token: str, all_versions: bool = False) -> list:
    data = graphql_request(
        url,
        CHARTS4EXPORT_QUERY,
        {"allVersions": all_versions},
        token=token,
    )
    return data["charts4export"]


def main() -> None:
    parser = argparse.ArgumentParser(description="Download chart data from the MedBench Platform")
    parser.add_argument("--url", default=os.environ.get("MEDBENCH_URL", "http://localhost:4000/graphql"))
    parser.add_argument("--token", default=os.environ.get("MEDBENCH_TOKEN"))
    parser.add_argument("--email", default=os.environ.get("MEDBENCH_EMAIL"))
    parser.add_argument("--password", default=os.environ.get("MEDBENCH_PASSWORD"))
    parser.add_argument("--out", default="data/output/allData.json")
    parser.add_argument("--all-versions", action="store_true", help="Include all case versions (default: latest only)")
    args = parser.parse_args()

    token = args.token
    if not token:
        if not args.email or not args.password:
            parser.error("Provide --token or both --email and --password (or set MEDBENCH_TOKEN / MEDBENCH_EMAIL+MEDBENCH_PASSWORD env vars)")
        print(f"Logging in as {args.email}...")
        token = get_token(args.url, args.email, args.password)
        print("Login successful.")

    print(f"Downloading charts from {args.url}...")
    charts = download_charts(args.url, token, all_versions=args.all_versions)
    print(f"Downloaded {len(charts)} cases ({sum(len(c.get('charts', [])) for c in charts)} chart translations).")

    out_path = Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)
    with open(out_path, "w", encoding="utf-8") as f:
        json.dump(charts, f, ensure_ascii=False, indent=2)
    print(f"Saved to {out_path}")


if __name__ == "__main__":
    main()
