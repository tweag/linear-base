import sys
import pandas as pd
from decimal import Decimal, getcontext

def format_significant_decimal(x, digits=5):
    if pd.isna(x):
        return ''
    if x == 0:
        return "0"
    
    getcontext().prec = digits + 5
    d = Decimal(str(x))
    
    exponent = d.adjusted()
    shift = digits - exponent - 1
    rounded = d.scaleb(shift).quantize(Decimal('1')).scaleb(-shift)
    s = format(rounded.normalize(), 'f')
    return s

def format_dataframe_numbers(df, digits=5):
    for col in df.select_dtypes(include=["number"]).columns:
        df[col] = df[col].apply(lambda x: format_significant_decimal(x, digits))
    return df

def usage():
    print("Usage:")
    print("  python script.py manual_bench.csv averaged_manual_bench.csv")
    print("  OR")
    print("  python script.py manual_bench.csv averaged_manual_bench.csv auto_bench.csv updated_auto_bench.csv columns_to_patch")
    print("    where columns_to_patch is a comma-separated list like: AutoCol1:ManualCol1,AutoCol2:ManualCol2 or just AutoCol1,AutoCol2 (for identity mapping)")
    sys.exit(1)

if not (4 <= len(sys.argv) <= 6):
    usage()

manual_path = sys.argv[1]
averaged_manual_path = sys.argv[2]

patching_enabled = False
if len(sys.argv) == 6:
    patching_enabled = True
    auto_path = sys.argv[3]
    updated_auto_path = sys.argv[4]
    raw_column_mappings = sys.argv[5].split(",")

    # Parse mappings like "A:B" or "A"
    column_mappings = []
    for entry in raw_column_mappings:
        if ':' in entry:
            auto_col, manual_col = entry.split(":", 1)
        else:
            auto_col = manual_col = entry
        column_mappings.append((auto_col.strip(), manual_col.strip()))
elif len(sys.argv) == 3:
    column_mappings = []
else:
    usage()

# Load and process manual benchmark
manual_df = pd.read_csv(manual_path)
if "Iteration" in manual_df.columns:
    manual_df = manual_df.drop(columns=["Iteration"])

aggregated = (
    manual_df
    .groupby(["Test Description", "Size", "Method"], as_index=False)
    .mean(numeric_only=True)
)

aggregated = format_dataframe_numbers(aggregated, digits=5)
aggregated.to_csv(averaged_manual_path, index=False)
print(f"Averaged manual benchmark written to: {averaged_manual_path}")

if patching_enabled:
    auto_df = pd.read_csv(auto_path)

    # Build lookup from aggregated benchmark
    lookup = {}
    for _, row in aggregated.iterrows():
        key = (row["Test Description"], row["Size"], row["Method"])
        lookup[key] = row

    # Patch auto benchmark
    for idx, row in auto_df.iterrows():
        key = (row["Test Description"], row["Size"], row["Method"])
        if key in lookup:
            for auto_col, manual_col in column_mappings:
                if manual_col in lookup[key]:
                    auto_df.at[idx, auto_col] = lookup[key][manual_col]
                else:
                    print(f"Warning: Column '{manual_col}' not found for row {key}")
        else:
            print(f"Warning: No manual benchmark found for: {key}")

    auto_df = format_dataframe_numbers(auto_df, digits=5)
    auto_df.to_csv(updated_auto_path, index=False)
    print(f"Patched auto benchmark written to: {updated_auto_path}")
