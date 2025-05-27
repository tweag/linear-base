import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.cm as cm
import matplotlib.colors as mcolors
import colorsys
import sys

# Font config for LaTeX-style labels
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
})

# Constants
PEAK_BASE = 69         # Subtracted from peak memory values (in MB)
PEAK_THRESHOLD = 3    # Minimum required baseline peak after subtraction (in MB)


def dedup_list(seq):
    """Remove duplicates from a list while preserving order."""
    seen = set()
    result = []
    for item in seq:
        if item not in seen:
            seen.add(item)
            result.append(item)
    return result

# Adjust variant colors
def adjust_color(color, variant):
    """Lighten or darken color depending on variant."""
    r, g, b = mcolors.to_rgb(color)
    h, l, s = colorsys.rgb_to_hls(r, g, b)
    if variant == "force":
        l = max(0, min(1, l * 0.9))  # Darker
    elif variant == "copyIntoReg":
        l = max(0, min(1, l * 1.3))  # Lighter
    # Convert back to RGB
    r_new, g_new, b_new = colorsys.hls_to_rgb(h, l, s)
    return (r_new, g_new, b_new)

def draw(df, test_description, methods, method_baseline, title, output_file=None):
    # Start fresh
    plt.close('all')

    filtered_df = df[df["Test Description"] == test_description]
    filtered_df = filtered_df[filtered_df["Method"].isin(methods)]

    # Parse size field like '2^10' → 1024
    filtered_df["SizeNum"] = filtered_df["Size"].apply(lambda s: 2 ** int(s.split("^")[1]))
    filtered_df = filtered_df.sort_values(by=["Method", "SizeNum"])

    # Pivot tables
    time_pivot = filtered_df.pivot(index="SizeNum", columns="Method", values="Time (ms)")
    alloc_pivot = filtered_df.pivot(index="SizeNum", columns="Method", values="Allocated (MB)")
    peak_raw = filtered_df.pivot(index="SizeNum", columns="Method", values="Peak (MB)")
    copied_pivot = filtered_df.pivot(index="SizeNum", columns="Method", values="Copied (MB)")

    # Subtract PEAK_BASE from peak values
    peak_pivot = peak_raw - PEAK_BASE

    # Filter memory-related data using PEAK_THRESHOLD
    baseline_peak = peak_pivot[method_baseline]
    valid_mask = baseline_peak > PEAK_THRESHOLD
    valid_sizes = baseline_peak.index[valid_mask]

    # Prepare memory data
    alloc_mem = alloc_pivot.loc[valid_sizes]
    peak_mem = peak_pivot.loc[valid_sizes]
    copied_mem = copied_pivot.loc[valid_sizes]
    baseline_peak_filtered = baseline_peak.loc[valid_sizes]

    # Assign base colors by prefix (e.g. mapL)
    base_names = dedup_list([m.split('.')[0] for m in methods])
    base_cmap = cm.get_cmap('tab10')
    base_colors = {base: base_cmap(i) for i, base in enumerate(base_names)}

    method_colors = {}
    for method in methods:
        parts = method.split('.')
        base = parts[0]
        variant = parts[1] if len(parts) > 1 else ''
        method_colors[method] = adjust_color(base_colors[base], variant)

    # X-axis ticks
    xtick_powers = list(range(10, 23, 3))
    xticks = [2 ** p for p in xtick_powers]
    xtick_labels = [f"$2^{{{p}}}$" for p in xtick_powers]

    # Create plots
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6), sharex=False)


    # --- Time plot (all sizes included) ---
    for method in methods:
        rel_time = time_pivot[method] / time_pivot[method_baseline]
        ax1.plot(time_pivot.index, rel_time, marker='o', color=method_colors[method],
                label=method, linewidth=1.5,
                alpha=1.0)

    ax1.set_xscale("log", base=2)
    ax1.set_xticks(xticks)
    ax1.set_xticklabels(xtick_labels)
    ax1.axhline(1.0, color='gray', linestyle='--', linewidth=1)
    ax1.set_ylabel(f"Relative time (vs. {method_baseline})")
    ax1.set_xlabel("Input size (nb. of elements)")
    ax1.set_title("Time (lower is better)")
    ax1.grid(True, which="both", linestyle="--", linewidth=0.5)
    ax1.legend()

    # --- Memory plot (filtered sizes only) ---
    for method in methods:
        rel_peak = peak_mem[method] / baseline_peak_filtered
        rel_alloc = alloc_mem[method] / baseline_peak_filtered
        rel_copied = copied_mem[method] / baseline_peak_filtered

        color = method_colors[method]
        style = {'linewidth': 1.5,
                'alpha': 1.0}

        valid_peak = rel_peak > 0
        valid_alloc = rel_alloc > 0
        valid_copied = rel_copied > 0

        ax2.plot(peak_mem.index[valid_peak], rel_peak[valid_peak], marker='o', linestyle='-', color=color,
                label=f"{method} peak", **style)
        ax2.plot(alloc_mem.index[valid_alloc], rel_alloc[valid_alloc], marker='+', linestyle='--', color=color,
                label=f"{method} allocated", **style)
        ax2.plot(copied_mem.index[valid_copied], rel_copied[valid_copied], marker='d', linestyle=':', color=color,
                label=f"{method} copied", **style)

    ax2.set_xscale("log", base=2)
    ax2.set_xticks(xticks)
    ax2.set_xticklabels(xtick_labels)
    ax2.axhline(1.0, color='gray', linestyle='--', linewidth=1)
    ax2.set_ylabel(f"Relative memory usage (vs. {method_baseline} peak)")
    ax2.set_xlabel("Input size (nb. of elements)")
    ax2.set_title("Memory (lower is better)")
    ax2.grid(True, which="both", linestyle="--", linewidth=0.5)
    ax2.legend()

    # Super caption
    fig.text(0.5, 0.01, title, ha='center', fontsize=13)

    plt.tight_layout(rect=[0, 0.05, 1, 0.95])
    if output_file:
        plt.savefig(output_file, bbox_inches='tight')
    else:
        plt.show()

###############################################################################

# Load CSV
df = pd.read_csv(sys.argv[1])

###############################################################################
#                                    BFT                                      #
###############################################################################
draw(
    df,
    "DPS interface for compact regions/Breadth-first tree traversal",
    [
        "phasesRelabel.force",
        "phasesRelabel.copyIntoReg",
        "dpsRelabel",
    ],
    "phasesRelabel.force",
    "Benchmark of breadth-first traversal relabeling",
    "plot_bft.pdf"
)

###############################################################################
#                                    MAP                                      #
###############################################################################
draw(
    df,
    "DPS interface for compact regions/map on List",
    [
        "mapL.force",
        # "mapL.copyIntoReg",
        # "mapS.force",
        # "mapS.copyIntoReg",
        "mapSH.force",
        "mapSH.copyIntoReg",
        # "mapST.force",
        # "mapST.copyIntoReg",
        # "mapTRL.force",
        # "mapTRL.copyIntoReg",
        "mapTRS.force",
        "mapTRS.copyIntoReg",
        # "mapTRSH.force",
        # "mapTRSH.copyIntoReg",
        # "mapTRST.force",
        # "mapTRST.copyIntoReg",
        # "mapDestTRL",
        "mapDestTRS",
        # "mapDestFL",
        # "mapDestFLS",
        # "mapDestFSL",
        "mapDestFS"
    ],
    "mapL.force",
    "Benchmark of map on list",
    "plot_map.pdf"
)

###############################################################################
#                                    CONCAT                                   #
###############################################################################
draw(
    df,
    "DPS interface for compact regions/List and DList concatenation",
    [
        "concatRight.force",
        # "concatRight.copyIntoReg",
        "differenceListNaiveLeft.force",
        "differenceListNaiveLeft.copyIntoReg",
        "differenceListDestLeft",
    ],
    "concatRight.force",
    "Benchmark of iterated concatenations on lists and difference lists",
    "plot_concat.pdf"
)

###############################################################################
#                                    QUEUE                                    #
###############################################################################
draw(
    df,
    "DPS interface for compact regions/Queue enqueue operations",
    [
        "naiveImpl",
        "funcImpl",
        "destImpl",
    ],
    "naiveImpl",
    "Benchmark of enqueue operations on queue",
    "plot_queue.pdf"
)

###############################################################################
#                                    PARSER                                   #
###############################################################################
draw(
    df,
    "DPS interface for compact regions/S-expression parser",
    [
        "parseWithoutDest.force",
        "parseWithoutDest.copyIntoReg",
        "parseWithDest",
    ],
    "parseWithoutDest.force",
    "Benchmark of S-expression parser",
    "plot_parser.pdf"
)
