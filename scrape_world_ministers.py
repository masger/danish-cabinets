#!/usr/bin/env python3
"""
Scrape world ministers from Wikidata using SPARQL queries.
This script fetches cabinet ministers from all countries with their:
- Name
- Position/Ministry
- Country
- Start date
- End date
- Political party (when available)

Data sources:
- Wikidata: https://www.wikidata.org/
- WikiProject Every Politician: https://www.wikidata.org/wiki/Wikidata:WikiProject_every_politician
"""

import requests
import pandas as pd
from datetime import datetime
import time
import sys

WIKIDATA_SPARQL_ENDPOINT = "https://query.wikidata.org/sparql"

# List of cabinet-level positions to query (Wikidata Q-IDs)
# These are the main ministerial position types
CABINET_POSITION_TYPES = [
    "Q83307",      # minister (general)
    "Q637591",     # Secretary of State
    "Q132050",     # Prime Minister
    "Q14212",      # President
    "Q116862",     # Chancellor
    "Q1553195",    # Deputy Prime Minister
    "Q7330070",    # Minister of Finance
    "Q534452",     # Minister of Foreign Affairs
    "Q2282666",    # Minister of Defence
    "Q16530238",   # Minister of the Interior
    "Q2824523",    # Minister of Justice
    "Q16533",      # Chief Justice
]


def query_wikidata(sparql_query: str, max_retries: int = 3) -> dict:
    """Execute a SPARQL query against Wikidata endpoint with retries."""
    headers = {
        "Accept": "application/sparql-results+json",
        "User-Agent": "WorldMinistersScraper/1.0 (https://github.com/danish-cabinets; contact@example.com)"
    }

    for attempt in range(max_retries):
        try:
            response = requests.get(
                WIKIDATA_SPARQL_ENDPOINT,
                params={"query": sparql_query},
                headers=headers,
                timeout=120
            )
            response.raise_for_status()
            return response.json()
        except requests.exceptions.RequestException as e:
            wait_time = 2 ** (attempt + 1)
            print(f"Query failed (attempt {attempt + 1}/{max_retries}): {e}")
            if attempt < max_retries - 1:
                print(f"Retrying in {wait_time} seconds...")
                time.sleep(wait_time)
            else:
                raise
    return {}


def get_ministers_by_country(country_qid: str, country_name: str) -> pd.DataFrame:
    """Fetch all ministers for a specific country."""

    # SPARQL query to get cabinet members for a country
    sparql_query = f"""
    SELECT DISTINCT ?person ?personLabel ?position ?positionLabel ?partyLabel ?startDate ?endDate ?cabinetLabel
    WHERE {{
      # Get people who held positions in government of this country
      ?person wdt:P39 ?position .
      ?position wdt:P17 wd:{country_qid} .

      # Position should be an instance of or subclass of minister/government position
      {{ ?position wdt:P31/wdt:P279* wd:Q83307 . }}
      UNION
      {{ ?position wdt:P31/wdt:P279* wd:Q4164871 . }}  # government position
      UNION
      {{ ?position wdt:P31/wdt:P279* wd:Q132050 . }}   # prime minister
      UNION
      {{ ?position wdt:P31/wdt:P279* wd:Q14212 . }}    # president

      # Get the position statement with qualifiers
      ?person p:P39 ?statement .
      ?statement ps:P39 ?position .

      # Get start and end dates (optional)
      OPTIONAL {{ ?statement pq:P580 ?startDate . }}
      OPTIONAL {{ ?statement pq:P582 ?endDate . }}

      # Get cabinet/government (optional)
      OPTIONAL {{ ?statement pq:P5054 ?cabinet . }}

      # Get party affiliation (optional)
      OPTIONAL {{ ?person wdt:P102 ?party . }}

      SERVICE wikibase:label {{ bd:serviceParam wikibase:language "en,da,de,fr,es,it,pt,nl,sv,no,fi". }}
    }}
    ORDER BY ?startDate
    """

    try:
        result = query_wikidata(sparql_query)
        records = []

        for item in result.get("results", {}).get("bindings", []):
            record = {
                "navn": item.get("personLabel", {}).get("value", ""),
                "ministerpost": item.get("positionLabel", {}).get("value", ""),
                "parti": item.get("partyLabel", {}).get("value", "") if "partyLabel" in item else None,
                "start": item.get("startDate", {}).get("value", "")[:10] if "startDate" in item else "",
                "stop": item.get("endDate", {}).get("value", "")[:10] if "endDate" in item else "",
                "regering": item.get("cabinetLabel", {}).get("value", "") if "cabinetLabel" in item else "",
                "country": country_name,
                "country_qid": country_qid
            }
            records.append(record)

        return pd.DataFrame(records)

    except Exception as e:
        print(f"Error fetching ministers for {country_name}: {e}")
        return pd.DataFrame()


def get_all_countries() -> list:
    """Get list of all countries from Wikidata."""

    sparql_query = """
    SELECT DISTINCT ?country ?countryLabel
    WHERE {
      ?country wdt:P31 wd:Q6256 .  # instance of country
      ?country wdt:P31 ?type .
      FILTER NOT EXISTS { ?country wdt:P576 ?dissolved . }  # exclude dissolved countries

      SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
    }
    ORDER BY ?countryLabel
    """

    result = query_wikidata(sparql_query)
    countries = []

    for item in result.get("results", {}).get("bindings", []):
        country_uri = item.get("country", {}).get("value", "")
        country_qid = country_uri.split("/")[-1] if country_uri else ""
        country_name = item.get("countryLabel", {}).get("value", "")

        if country_qid and country_name and not country_name.startswith("Q"):
            countries.append({"qid": country_qid, "name": country_name})

    return countries


def get_all_current_ministers() -> pd.DataFrame:
    """Fetch all current cabinet ministers worldwide in a single query."""

    print("Fetching all current ministers worldwide...")

    sparql_query = """
    SELECT DISTINCT ?person ?personLabel ?position ?positionLabel ?countryLabel ?partyLabel ?startDate ?cabinetLabel
    WHERE {
      # Get people currently holding government positions
      ?person p:P39 ?statement .
      ?statement ps:P39 ?position .

      # Position should be in a country's government
      ?position wdt:P17 ?country .
      ?country wdt:P31 wd:Q6256 .  # instance of country

      # Position should be ministerial/government position
      { ?position wdt:P31/wdt:P279* wd:Q83307 . }    # minister
      UNION
      { ?position wdt:P31/wdt:P279* wd:Q4164871 . }  # government position
      UNION
      { ?position wdt:P31/wdt:P279* wd:Q132050 . }   # prime minister

      # Only current positions (no end date)
      FILTER NOT EXISTS { ?statement pq:P582 ?endDate . }

      # Get start date (optional)
      OPTIONAL { ?statement pq:P580 ?startDate . }

      # Get cabinet (optional)
      OPTIONAL { ?statement pq:P5054 ?cabinet . }

      # Get party (optional)
      OPTIONAL { ?person wdt:P102 ?party . }

      SERVICE wikibase:label { bd:serviceParam wikibase:language "en,da,de,fr,es". }
    }
    ORDER BY ?countryLabel ?positionLabel
    """

    try:
        result = query_wikidata(sparql_query)
        records = []

        for item in result.get("results", {}).get("bindings", []):
            record = {
                "navn": item.get("personLabel", {}).get("value", ""),
                "ministerpost": item.get("positionLabel", {}).get("value", ""),
                "parti": item.get("partyLabel", {}).get("value", "") if "partyLabel" in item else None,
                "start": item.get("startDate", {}).get("value", "")[:10] if "startDate" in item else "",
                "stop": "",  # Current ministers have no end date
                "regering": item.get("cabinetLabel", {}).get("value", "") if "cabinetLabel" in item else "",
                "country": item.get("countryLabel", {}).get("value", ""),
            }
            records.append(record)

        df = pd.DataFrame(records)
        print(f"Found {len(df)} current ministers")
        return df

    except Exception as e:
        print(f"Error fetching current ministers: {e}")
        return pd.DataFrame()


def get_historical_ministers_batch(offset: int = 0, limit: int = 10000) -> pd.DataFrame:
    """Fetch historical ministers in batches."""

    print(f"Fetching historical ministers (offset: {offset}, limit: {limit})...")

    sparql_query = f"""
    SELECT DISTINCT ?person ?personLabel ?position ?positionLabel ?countryLabel ?partyLabel ?startDate ?endDate ?cabinetLabel
    WHERE {{
      # Get people who held government positions
      ?person p:P39 ?statement .
      ?statement ps:P39 ?position .

      # Position should be in a country's government
      ?position wdt:P17 ?country .
      ?country wdt:P31 wd:Q6256 .

      # Position should be ministerial/government position
      {{ ?position wdt:P31/wdt:P279* wd:Q83307 . }}
      UNION
      {{ ?position wdt:P31/wdt:P279* wd:Q4164871 . }}
      UNION
      {{ ?position wdt:P31/wdt:P279* wd:Q132050 . }}

      # Get dates (at least one must exist)
      OPTIONAL {{ ?statement pq:P580 ?startDate . }}
      OPTIONAL {{ ?statement pq:P582 ?endDate . }}
      FILTER(BOUND(?startDate) || BOUND(?endDate))

      # Get cabinet (optional)
      OPTIONAL {{ ?statement pq:P5054 ?cabinet . }}

      # Get party (optional)
      OPTIONAL {{ ?person wdt:P102 ?party . }}

      SERVICE wikibase:label {{ bd:serviceParam wikibase:language "en,da,de,fr,es". }}
    }}
    ORDER BY ?countryLabel ?startDate
    LIMIT {limit}
    OFFSET {offset}
    """

    try:
        result = query_wikidata(sparql_query)
        records = []

        for item in result.get("results", {}).get("bindings", []):
            record = {
                "navn": item.get("personLabel", {}).get("value", ""),
                "ministerpost": item.get("positionLabel", {}).get("value", ""),
                "parti": item.get("partyLabel", {}).get("value", "") if "partyLabel" in item else None,
                "start": item.get("startDate", {}).get("value", "")[:10] if "startDate" in item else "",
                "stop": item.get("endDate", {}).get("value", "")[:10] if "endDate" in item else "",
                "regering": item.get("cabinetLabel", {}).get("value", "") if "cabinetLabel" in item else "",
                "country": item.get("countryLabel", {}).get("value", ""),
            }
            records.append(record)

        df = pd.DataFrame(records)
        print(f"Fetched {len(df)} records")
        return df

    except Exception as e:
        print(f"Error fetching historical ministers: {e}")
        return pd.DataFrame()


def main():
    """Main function to scrape world ministers data."""

    print("=" * 60)
    print("World Ministers Scraper - Wikidata")
    print("=" * 60)
    print()

    all_ministers = []

    # Option 1: Get all current ministers
    print("\n[1/2] Fetching current ministers...")
    current = get_all_current_ministers()
    if not current.empty:
        current["is_current"] = True
        all_ministers.append(current)

    # Option 2: Get historical ministers in batches
    print("\n[2/2] Fetching historical ministers...")
    offset = 0
    batch_size = 10000
    max_records = 100000  # Limit to avoid timeout

    while offset < max_records:
        batch = get_historical_ministers_batch(offset=offset, limit=batch_size)
        if batch.empty:
            break

        batch["is_current"] = False
        all_ministers.append(batch)

        if len(batch) < batch_size:
            break

        offset += batch_size
        time.sleep(2)  # Be nice to Wikidata servers

    # Combine all data
    if all_ministers:
        df = pd.concat(all_ministers, ignore_index=True)

        # Remove duplicates
        df = df.drop_duplicates(subset=["navn", "ministerpost", "country", "start"])

        # Sort by country and start date
        df = df.sort_values(["country", "start", "navn"])

        # Calculate time in office (in years)
        def calc_time(row):
            try:
                if row["start"] and row["stop"]:
                    start = datetime.strptime(row["start"], "%Y-%m-%d")
                    stop = datetime.strptime(row["stop"], "%Y-%m-%d")
                    return (stop - start).days / 365.25
                return None
            except:
                return None

        df["time"] = df.apply(calc_time, axis=1)

        # Save to CSV
        output_file = "world_ministers_wikidata.csv"
        df.to_csv(output_file, index=False)

        print()
        print("=" * 60)
        print(f"SUCCESS! Scraped {len(df)} minister records")
        print(f"Countries covered: {df['country'].nunique()}")
        print(f"Output saved to: {output_file}")
        print("=" * 60)

        # Show summary by country
        print("\nTop 20 countries by number of records:")
        print(df["country"].value_counts().head(20).to_string())

        return df
    else:
        print("No data fetched!")
        return pd.DataFrame()


if __name__ == "__main__":
    main()
