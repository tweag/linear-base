import re
import sys
import csv

# One strict regex for the entire log block
LOG_PATTERN = re.compile(r"""
^===\sRunning\s
(?P<test_desc>.+?)\.
(?P<size>2\^\d+)\.
(?P<method>[^(]+)
\s+\(iteration\s(?P<iteration>\d+)\)\s===\n

.*?
(?P<allocated>[\d,]+)\s+bytes\s+allocated\s+in\s+the\s+heap\n
\s+(?P<copied>[\d,]+)\s+bytes\s+copied\s+during\s+GC\n
\s+(?P<max_residency>[\d,]+)\s+bytes\s+maximum\s+residency.*?\n
.*?\n
\s+(?P<peak>\d+)\s+MiB\s+total\s+memory\s+in\s+use[^\n]*\n

(?:.|\n)*?
GC\s+time\s+(?P<gc_time>\d+\.\d+)s.*?\n
(?:.|\n)*?
Total\s+time\s+(?P<total_time>\d+\.\d+)s
""", re.VERBOSE | re.MULTILINE)

# Convert byte string with commas to MB float
def bytes_to_mb(value):
    return int(value.replace(",", "")) / (1024 * 1024)

# Parse the log content into structured rows
def parse_log(text):
    rows = []
    for match in LOG_PATTERN.finditer(text):
        d = match.groupdict()
        row = {
            "Test Description": d["test_desc"].replace(".", "/")[4:],
            "Size": d["size"],
            "Method": d["method"],
            "Iteration": int(d["iteration"]),
            "Time (ms)": float(d["total_time"]) * 1000,
            "GC Time (ms)": float(d["gc_time"]) * 1000,
            "Allocated (MB)": bytes_to_mb(d["allocated"]),
            "Copied (MB)": bytes_to_mb(d["copied"]),
            "Max Live Gen 1 (MB)": bytes_to_mb(d["max_residency"]),
            "Peak (MB)": int(d["peak"]),
        }
        rows.append(row)
    return rows

# Write the parsed rows as CSV to a file
def write_csv(rows, output_path):
    headers = [
        "Test Description", "Size", "Method", "Iteration",
        "Time (ms)", "GC Time (ms)", "Allocated (MB)",
        "Copied (MB)", "Max Live Gen 1 (MB)", "Peak (MB)"
    ]
    with open(output_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=headers)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)

# Entry point
if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: python parse_gc_log.py <input_log_file> <output_csv_file>")
        sys.exit(1)

    input_path = sys.argv[1]
    output_path = sys.argv[2]

    with open(input_path, "r") as f:
        log_text = f.read()

    rows = parse_log(log_text)
    write_csv(rows, output_path)
    print(f"Wrote {len(rows)} rows to {output_path}")
