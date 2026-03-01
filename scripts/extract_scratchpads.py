import os
import glob
import re
import pandas as pd
from io import StringIO

scratchpad_dir = r"C:\Users\mand0579\.gemini\antigravity\brain\4730bcc2-423f-4d7a-ac28-769117d85078\browser"
data_dir = r"c:\Users\mand0579\OneDrive - Region Hovedstaden (1)\Projekter\danish-cabinets\data\international"

for f in glob.glob(os.path.join(scratchpad_dir, "scratchpad_*.md")):
    with open(f, 'r', encoding='utf-8') as file:
        content = file.read()

    match = re.search(r'```csv\s+(.*?)\s+```', content, re.DOTALL)
    csv_data = ""
    if match:
        csv_data = match.group(1).strip()
    else:
        lines = content.split('\n')
        start_idx = -1
        for i, line in enumerate(lines):
            if line.startswith('country,portfolio') or line.startswith('"country","portfolio"'):
                start_idx = i
                break
        if start_idx != -1:
            csv_data = '\n'.join(lines[start_idx:]).strip()

    if csv_data:
        try:
            df = pd.read_csv(StringIO(csv_data))
            if not df.empty and 'country' in df.columns and 'portfolio' in df.columns:
                country = df['country'].iloc[0]
                portfolio = df['portfolio'].iloc[0]
                out_name = f"{country}_{portfolio}_scraped.csv"
                out_path = os.path.join(data_dir, out_name)
                df.to_csv(out_path, index=False)
                print(f"Saved {out_path} from {os.path.basename(f)}")
                os.remove(f)
        except Exception as e:
            print(f"Error parsing {f}: {e}")
