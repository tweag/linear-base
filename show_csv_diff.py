#!/usr/bin/env python3
import sys
import pandas as pd
import numpy as np

def usage():
    print("Usage: python show_csv_diff.py old.csv new.csv")
    sys.exit(1)

if len(sys.argv) != 3:
    usage()

old_path = sys.argv[1]
new_path = sys.argv[2]

# Load CSVs
old_df = pd.read_csv(old_path)
new_df = pd.read_csv(new_path)

key_cols = ["Test Description", "Size", "Method"]

# Check keys present in both
old_keys = set(tuple(x) for x in old_df[key_cols].values)
new_keys = set(tuple(x) for x in new_df[key_cols].values)
common_keys = old_keys.intersection(new_keys)

if not common_keys:
    print("Warning: No common keys found between files.")
    sys.exit(0)

# Index dataframes by keys for faster lookup
old_df.set_index(key_cols, inplace=True)
new_df.set_index(key_cols, inplace=True)

numeric_cols = [col for col in old_df.columns if col not in key_cols and np.issubdtype(old_df[col].dtype, np.number)]

differences_found = False

for key in sorted(common_keys):
    if key not in old_df.index or key not in new_df.index:
        continue  # skip if missing

    old_row = old_df.loc[key]
    new_row = new_df.loc[key]

    # If single row, make sure they are Series
    if isinstance(old_row, pd.DataFrame):
        old_row = old_row.iloc[0]
    if isinstance(new_row, pd.DataFrame):
        new_row = new_row.iloc[0]

    diffs = {}
    for col in numeric_cols:
        old_val = old_row[col]
        new_val = new_row[col]

        # Compare considering possible NaNs
        if pd.isna(old_val) and pd.isna(new_val):
            continue
        if pd.isna(old_val) or pd.isna(new_val) or old_val != new_val:
            diffs[col] = (old_val, new_val)

    if diffs:
        differences_found = True
        print(f"\nDifference at Test Description='{key[0]}', Size='{key[1]}', Method='{key[2]}':")
        for col, (old_val, new_val) in diffs.items():
            print(f"  {col}: old = {old_val}, new = {new_val}")

if not differences_found:
    print("No differences found in numeric columns for common entries.")
