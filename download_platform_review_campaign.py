"""Download a MedBench review campaign with charts, summaries, and reviews.

The export is intended for reproducible paper analysis. It selects one review
campaign, bundles the campaign chart material with all admin-visible reviews for
that campaign, and writes both a JSON artifact and a flattened CSV.

Usage:
    python download_platform_review_campaign.py \
        --url https://label.cairlab.ki.se/graphql \
        --email admin@example.com \
        --campaign-label "PDQI+ review"

    python download_platform_review_campaign.py \
        --url http://localhost:4000/graphql \
        --token <jwt_token> \
        --campaign-id <campaign_id>
"""

from __future__ import annotations

import argparse
import json
from pathlib import Path

from python_helpers.platform_graphql import (
    add_platform_auth_args,
    graphql_request,
    resolve_token,
)
from python_helpers.review_campaign_export import (
    enrich_campaign_charts,
    flatten_reviews,
    resolve_campaign,
    slugify,
)
from python_helpers.review_campaign_csv import write_csv
from python_helpers.review_campaign_queries import (
    ALL_REVIEWS_QUERY,
    CHARTS4EXPORT_QUERY,
    REVIEW_CAMPAIGNS_QUERY,
)


def main() -> None:
    """Download the selected campaign and write JSON plus flattened CSV outputs."""
    parser = argparse.ArgumentParser(
        description="Download a review campaign with chart material and PDQI+ reviews"
    )
    add_platform_auth_args(parser)
    parser.add_argument("--campaign-label", default=None)
    parser.add_argument("--campaign-id", default=None)
    parser.add_argument("--out", default=None, help="JSON output path")
    parser.add_argument("--csv-out", default=None, help="Flattened CSV output path")
    args = parser.parse_args()

    token = resolve_token(args, parser)

    print(f"Downloading campaigns from {args.url}...")
    campaigns_data = graphql_request(
        args.url, REVIEW_CAMPAIGNS_QUERY, {"onlyActive": False}, token=token
    )
    campaigns = campaigns_data["reviewCampaigns"]
    campaign = resolve_campaign(campaigns, args.campaign_label, args.campaign_id)
    slug = slugify(campaign["label"])
    print(f"Selected campaign: {campaign['label']} ({campaign['id']})")

    print("Downloading chart export for reproducible chart/fact context...")
    charts_data = graphql_request(
        args.url, CHARTS4EXPORT_QUERY, {"allVersions": False}, token=token
    )
    enriched_campaign = enrich_campaign_charts(campaign, charts_data["charts4export"])

    print("Downloading all reviews...")
    reviews_data = graphql_request(args.url, ALL_REVIEWS_QUERY, {}, token=token)
    selected_reviews = [
        review
        for review in reviews_data["allReviews"]
        if review.get("campaign", {}).get("id") == campaign["id"]
    ]
    flat_reviews = flatten_reviews(enriched_campaign, selected_reviews)

    export = {
        "campaign": enriched_campaign,
        "reviews": selected_reviews,
        "reviewsFlat": flat_reviews,
        "metadata": {
            "sourceUrl": args.url,
            "campaignId": campaign["id"],
            "campaignLabel": campaign["label"],
            "reviewCount": len(selected_reviews),
            "flatReviewCount": len(flat_reviews),
            "chartCount": len(enriched_campaign.get("charts", [])),
        },
    }

    json_path = Path(args.out or f"data/output/reviewCampaign_{slug}.json")
    json_path.parent.mkdir(parents=True, exist_ok=True)
    with open(json_path, "w", encoding="utf-8") as f:
        json.dump(export, f, ensure_ascii=False, indent=2)
    print(f"Saved JSON export to {json_path}")

    csv_path = Path(args.csv_out or f"results/review_metrics_{slug}.csv")
    write_csv(flat_reviews, csv_path)
    print(f"Saved flattened review metrics to {csv_path}")
    print(
        f"Exported {len(enriched_campaign.get('charts', []))} charts, "
        f"{sum(len(chart.get('summaries', [])) for chart in enriched_campaign.get('charts', []))} summaries, "
        f"and {len(selected_reviews)} reviews."
    )


if __name__ == "__main__":
    main()
