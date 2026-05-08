"""Shared GraphQL helpers for MedBench Platform scripts."""

import argparse
import getpass
import json
import os
import urllib.error
import urllib.request
from typing import Any


LOGIN_MUTATION = """
mutation Login($email: String!, $password: String!) {
  login(email: $email, password: $password) {
    accessToken
  }
}
"""


def graphql_request(
    url: str, query: str, variables: dict[str, Any], token: str | None = None
) -> dict[str, Any]:
    """Send a GraphQL POST request and return the response data object.

    Args:
        url: Full Platform GraphQL endpoint URL.
        query: GraphQL query or mutation string.
        variables: JSON-serializable GraphQL variables.
        token: Optional JWT access token. When provided, it is sent as a bearer token.

    Raises:
        RuntimeError: If the GraphQL response contains an ``errors`` key.

    Returns:
        The GraphQL ``data`` payload.
    """
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
    """Authenticate with email/password and return a Platform access token."""
    data = graphql_request(url, LOGIN_MUTATION, {"email": email, "password": password})
    return data["login"]["accessToken"]


def add_platform_auth_args(parser: argparse.ArgumentParser) -> None:
    """Add common Platform connection and authentication options to a parser.

    The defaults mirror the existing downloader scripts: URL and credentials can
    be supplied via ``MEDBENCH_URL``, ``MEDBENCH_TOKEN``, ``MEDBENCH_EMAIL``,
    and ``MEDBENCH_PASSWORD``.
    """
    parser.add_argument(
        "--url", default=os.environ.get("MEDBENCH_URL", "http://localhost:4000/graphql")
    )
    parser.add_argument("--token", default=os.environ.get("MEDBENCH_TOKEN"))
    parser.add_argument("--email", default=os.environ.get("MEDBENCH_EMAIL"))
    parser.add_argument(
        "--password",
        default=os.environ.get("MEDBENCH_PASSWORD"),
        help="Password for login; if omitted the script prompts securely when --email is provided",
    )


def resolve_token(args: argparse.Namespace, parser: argparse.ArgumentParser) -> str:
    """Resolve a JWT token from CLI args/env or by interactive login.

    Args:
        args: Parsed argparse namespace with the options added by
            ``add_platform_auth_args``.
        parser: Parser used to report a standard CLI error when neither token
            nor email is available.

    Returns:
        A JWT access token suitable for authenticated GraphQL requests.
    """
    token = args.token
    if token:
        return token

    if not args.email:
        parser.error(
            "Provide --token or --email (or set MEDBENCH_TOKEN / MEDBENCH_EMAIL env vars)"
        )
    if not args.password:
        args.password = getpass.getpass(f"Password for {args.email}: ")
    print(f"Logging in as {args.email}...")
    token = get_token(args.url, args.email, args.password)
    print("Login successful.")
    return token
