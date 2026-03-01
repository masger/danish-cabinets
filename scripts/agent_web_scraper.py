import requests
import json
import os
import csv
from datetime import datetime

print("Web Scraper Agent: Initializing Wikidata SPARQL query...")

query = """
SELECT ?person ?personLabel ?position ?positionLabel ?start ?end ?dob ?dod ?codLabel WHERE {
  VALUES ?position {
    wd:Q2492164 wd:Q954556 wd:Q248386 wd:Q1563273
    wd:Q3858450 wd:Q3858348 wd:Q3858340 wd:Q3858428
    wd:Q697204 wd:Q643801 wd:Q608441 wd:Q605688
  }
  ?person p:P39 ?statement .
  ?statement ps:P39 ?position .

  OPTIONAL { ?statement pq:P580 ?start . }
  OPTIONAL { ?statement pq:P582 ?end . }
  OPTIONAL { ?person wdt:P569 ?dob . }
  OPTIONAL { ?person wdt:P570 ?dod . }
  OPTIONAL { ?person wdt:P509 ?cod . }

  SERVICE wikibase:label { bd:serviceParam wikibase:language "[AUTO_LANGUAGE],en". }
}
"""

url = 'https://query.wikidata.org/sparql'
headers = {
    'User-Agent': 'BMJChristmasAgentBot/1.0 (danishcabinets@example.com) pd/1.0',
    'Accept': 'application/sparql-results+json'
}

print("Web Scraper: Executing SPARQL query against Wikidata (~15-30 seconds)...")

try:
    response = requests.get(url, params={'query': query}, headers=headers)
    response.raise_for_status()
    data = response.json()

    results = []
    for item in data['results']['bindings']:
        pos_name = item.get('positionLabel', {}).get('value', '')
        n = pos_name.lower()
        generic = 'Other'
        if 'health' in n: generic = 'Health'
        elif 'defence' in n or 'defense' in n: generic = 'Defense'
        elif 'chancellor' in n or 'finance' in n: generic = 'Finance'
        elif 'home' in n or 'interior' in n: generic = 'Interior'

        results.append({
            'person_id': item.get('person', {}).get('value', '').split('/')[-1],
            'person_name': item.get('personLabel', {}).get('value', ''),
            'position_id': item.get('position', {}).get('value', '').split('/')[-1],
            'position_name': pos_name,
            'portfolio_generic': generic,
            'start_date': item.get('start', {}).get('value', ''),
            'end_date': item.get('end', {}).get('value', ''),
            'dob': item.get('dob', {}).get('value', ''),
            'dod': item.get('dod', {}).get('value', ''),
            'cause_of_death': item.get('codLabel', {}).get('value', '')
        })

    os.makedirs('data/international', exist_ok=True)
    outpath = 'data/international/wikidata_portfolios.csv'

    keys = results[0].keys() if results else []
    with open(outpath, 'w', newline='', encoding='utf-8') as f:
        dict_writer = csv.DictWriter(f, fieldnames=keys)
        dict_writer.writeheader()
        dict_writer.writerows(results)

    print(f"Web Scraper: SUCCESS! Scraped {len(results)} records.")
    print(f"Data saved to {outpath}")

except Exception as e:
    print(f"Web Scraper: FAILED! Wikidata blocked us or timed out. Error: {e}")
