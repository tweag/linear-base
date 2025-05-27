#!/usr/bin/env python3

import re
import csv
import sys

from decimal import Decimal, getcontext

def format_significant_decimal(x, digits=3):
    if x == 0:
        return "0"
    
    getcontext().prec = digits + 5  # extra precision to avoid rounding issues
    d = Decimal(str(x))
    
    # Count how many digits before the first non-zero digit
    exponent = d.adjusted()  # like floor(log10(x))
    shift = digits - exponent - 1
    
    rounded = d.scaleb(shift).quantize(Decimal('1')).scaleb(-shift)
    
    # Strip trailing zeros after decimal but keep necessary precision
    s = format(rounded.normalize(), 'f')
    return s

def parse_time(value, unit):
    value = float(value)
    unit = unit.lower()
    if unit == 'ns':
        ms = value / 1_000_000
    elif unit == 'μs':
        ms = value / 1_000
    elif unit == 'ms':
        ms = value
    elif unit == 's':
        ms = value * 1_000
    else:
        raise ValueError(f"Unknown time unit: {unit}")

    return format_significant_decimal(ms)

def parse_size(value, unit):
    value = float(value)
    unit = unit.upper()
    if unit == 'B':
        mb = value / (1024 * 1024)
    elif unit == 'KB':
        mb = value / 1024
    elif unit == 'MB':
        mb = value
    elif unit == 'GB':
        mb = value * 1024
    else:
        raise ValueError(f"Unknown size unit: {unit}")
    
    return format_significant_decimal(mb)

# Regex to extract relevant blocks
block_pattern = re.compile(
    r"""All\n
        \s{2}(.+?)\n                 # Group 1: test category
        \s{4}(.+?)\n                 # Group 2: subcategory
        \s{6}(.+?)\n                 # Group 3: size label
        \s{8}(.+?):\s+OK\n        # Group 4: method
        \s{10}([\d.]+)\s*(ns|μs|ms|s)\s*±\s*[\d.]+\s*\w+,\s*
        ([\d.]+)\s*(B|KB|MB|GB)\s+allocated,\s*
        ([\d.]+)\s*(B|KB|MB|GB)\s+copied,\s*
        ([\d.]+)\s*(B|KB|MB|GB)\s+peak\s+memory
    """,
    re.VERBOSE | re.UNICODE
)

block_pattern2 = re.compile(
    r"""All\n
        \s{2}(.+?)\n                 # Group 1: test category
        \s{4}(.+?)\n                 # Group 2: size
        \s{6}(.+?):\s+OK\n        # Group 4: method
        \s{8}([\d.]+)\s*(ns|μs|ms|s)\s*±\s*[\d.]+\s*\w+,\s*
        ([\d.]+)\s*(B|KB|MB|GB)\s+allocated,\s*
        ([\d.]+)\s*(B|KB|MB|GB)\s+copied,\s*
        ([\d.]+)\s*(B|KB|MB|GB)\s+peak\s+memory
    """,
    re.VERBOSE  | re.UNICODE
)

input_filename = sys.argv[1]
output_filename = sys.argv[2]

with open(input_filename, "r") as file:
    content = file.read()

matches = block_pattern.findall(content)
matches2 = block_pattern2.findall(content)

with open(output_filename, "w", newline="") as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow([
        "Test Description", "Size", "Method",
        "Time (ms)", "Allocated (MB)", "Copied (MB)", "Peak (MB)"
    ])

    for (
        group1, group2, size_label, method,
        time_val, time_unit,
        alloc_val, alloc_unit,
        copy_val, copy_unit,
        peak_val, peak_unit
    ) in matches:

        test_description = f"{group1.strip()}/{group2.strip()}"
        size = size_label.strip()
        method_name = method.strip()
        time_ms = parse_time(time_val, time_unit)
        allocated_mb = parse_size(alloc_val, alloc_unit)
        copied_mb = parse_size(copy_val, copy_unit)
        peak_mb = parse_size(peak_val, peak_unit)

        writer.writerow([
            test_description, size, method_name,
            time_ms, allocated_mb, copied_mb, peak_mb
        ])

    for (
        group, size_label, method,
        time_val, time_unit,
        alloc_val, alloc_unit,
        copy_val, copy_unit,
        peak_val, peak_unit
    ) in matches2:

        test_description = group.strip()
        size = size_label.strip()
        method_name = method.strip()
        time_ms = parse_time(time_val, time_unit)
        allocated_mb = parse_size(alloc_val, alloc_unit)
        copied_mb = parse_size(copy_val, copy_unit)
        peak_mb = parse_size(peak_val, peak_unit)

        writer.writerow([
            test_description, size, method_name,
            time_ms, allocated_mb, copied_mb, peak_mb
        ])

print(f"Extracted {len(matches)+len(matches2)} entries to {output_filename}")
