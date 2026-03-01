import pandas as pd
import requests
import time
import glob
import os
import urllib.parse

HEADERS = {'User-Agent': 'SpinalTapBot/1.0 (research.project@example.com)'}

def get_wikidata_id(name, country_hint=""):
    print(f"Looking up {name}...")
    name_encoded = urllib.parse.quote(name)
    url = f"https://en.wikipedia.org/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&titles={name_encoded}&format=json"

    qid = None
    try:
        r = requests.get(url, headers=HEADERS)
        if r.status_code == 200:
            pages = r.json().get("query", {}).get("pages", {})
            for page_id, page_info in pages.items():
                if page_id != "-1" and "pageprops" in page_info:
                    qid = page_info["pageprops"].get("wikibase_item")
                    break
    except Exception as e:
        print(f"  API error on direct hit: {e}")

    if not qid:
        search_query = urllib.parse.quote(f"{name} politician {country_hint}".strip())
        search_url = f"https://en.wikipedia.org/w/api.php?action=query&list=search&srsearch={search_query}&format=json"
        try:
            sr = requests.get(search_url, headers=HEADERS)
            if sr.status_code == 200:
                search_results = sr.json().get("query", {}).get("search", [])
                if search_results:
                    title_encoded = urllib.parse.quote(search_results[0]["title"])
                    url2 = f"https://en.wikipedia.org/w/api.php?action=query&prop=pageprops&ppprop=wikibase_item&titles={title_encoded}&format=json"
                    r2 = requests.get(url2, headers=HEADERS)
                    if r2.status_code == 200:
                        pages2 = r2.json().get("query", {}).get("pages", {})
                        for page_id, page_info in pages2.items():
                            if page_id != "-1" and "pageprops" in page_info:
                                qid = page_info["pageprops"].get("wikibase_item")
                                break
        except Exception as e:
            print(f"  API error on search hit: {e}")

    return qid

def get_vital_stats(qid):
    if not qid:
        return None, None, None

    url = f"https://www.wikidata.org/wiki/Special:EntityData/{qid}.json"
    try:
        r = requests.get(url, headers=HEADERS)
        if r.status_code != 200:
            return None, None, None

        data = r.json()
        entities = data.get("entities", {})
        entity = entities.get(qid, {})
        claims = entity.get("claims", {})

        dob, dod, cause = None, None, None

        # P569 = date of birth
        if "P569" in claims:
            try:
                dob = claims["P569"][0]["mainsnak"]["datavalue"]["value"]["time"]
                if str(dob).startswith("+") or str(dob).startswith("-"):
                    dob = str(dob)[1:]
                dob = str(dob).split("T")[0]
            except Exception:
                pass

        # P570 = date of death
        if "P570" in claims:
            try:
                dod = claims["P570"][0]["mainsnak"]["datavalue"]["value"]["time"]
                if str(dod).startswith("+") or str(dod).startswith("-"):
                    dod = str(dod)[1:]
                dod = str(dod).split("T")[0]
            except Exception:
                pass

        # P509 = cause of death
        if "P509" in claims:
            try:
                cause_qid = claims["P509"][0]["mainsnak"]["datavalue"]["value"]["id"]
                c_url = f"https://www.wikidata.org/wiki/Special:EntityData/{cause_qid}.json"
                c_r = requests.get(c_url, headers=HEADERS)
                if c_r.status_code == 200:
                    cause = c_r.json().get("entities", {}).get(cause_qid, {}).get("labels", {}).get("en", {}).get("value")
            except Exception:
                pass

        return dob, dod, cause
    except Exception as e:
        print(f"  Error fetching {qid}: {e}")
        return None, None, None

def main():
    csv_files = glob.glob(r"data\international\*_scraped.csv")
    print(f"Found {len(csv_files)} scraped CSV files.")

    for file in csv_files:
        out_file = file.replace("_scraped.csv", "_enriched.csv")
        if os.path.exists(out_file):
            continue

        print(f"\n--- Processing {file} ---")
        df = pd.read_csv(file)

        dobs = []
        dods = []
        causes = []
        qids = []

        country_hint = df['country'].iloc[0] if not df.empty and 'country' in df.columns else ""

        for name in df['name']:
            qid = get_wikidata_id(name, country_hint)
            dob, dod, cause = get_vital_stats(qid)

            dobs.append(dob)
            dods.append(dod)
            causes.append(cause)
            qids.append(qid)

            print(f"  -> {qid}: DOB={dob}, DOD={dod}, Cause={cause}")
            time.sleep(0.1) # Be polite

        df['dob'] = dobs
        df['dod'] = dods
        df['cause_of_death'] = causes
        df['wikidata_id'] = qids

        out_file = file.replace("_scraped.csv", "_enriched.csv")
        df.to_csv(out_file, index=False)
        print(f"Saved {out_file}")

if __name__ == '__main__':
    main()
