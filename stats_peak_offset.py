import csv
from statistics import mean, median

# Replace this with your actual CSV file path if needed
csv_file_path = 'bench_patched_tweag-laptop.csv'

allocated_values = []

with open(csv_file_path, newline='') as csvfile:
    reader = csv.DictReader(csvfile)
    for row in reader:
        try:
            peak = float(row["Peak (MB)"])
            if peak == 6.0:
                allocated = float(row["Allocated (MB)"])
                allocated_values.append(allocated)
        except ValueError:
            continue  # Skip rows with malformed numbers

if allocated_values:
    avg_allocated = mean(allocated_values)
    median_allocated = median(allocated_values)
    print(f"Average Allocated (MB) for Peak = 6: {avg_allocated}")
    print(f"Median Allocated (MB) for Peak = 6: {median_allocated}")
else:
    print("No rows found with Peak (MB) = 6.")
