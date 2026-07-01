import pandas as pd
import requests

urls = {
    "Norway_Finance": "https://en.wikipedia.org/wiki/Minister_of_Finance_(Norway)",
    "Norway_Transport": "https://en.wikipedia.org/wiki/Minister_of_Transport_(Norway)",
    "Greece_Finance": "https://en.wikipedia.org/wiki/Minister_of_National_Economy_and_Finance_(Greece)",
    "Greece_Health": "https://en.wikipedia.org/wiki/Minister_for_Health_(Greece)",
    "Greece_Defense": "https://en.wikipedia.org/wiki/Minister_for_National_Defence_(Greece)",
    "Greece_Interior": "https://en.wikipedia.org/wiki/Minister_of_the_Interior_(Greece)",
    "Greece_Transport": "https://en.wikipedia.org/wiki/Minister_of_Infrastructure_and_Transport_(Greece)"
}

headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64)'
}

for name, url in urls.items():
    try:
        req = requests.get(url, headers=headers)
        dfs = pd.read_html(req.text)
        print(f"--- {name} ---")
        print(f"Found {len(dfs)} tables.")
        for i, df in enumerate(dfs):
            # Try to print column names to identify the right table
            print(f"Table {i} columns: {df.columns.tolist()[:5]}")
        print("\n")
    except Exception as e:
        print(f"Error on {name}: {e}")
