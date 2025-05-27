import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.cm as cm
import matplotlib.colors as mcolors
import colorsys
import sys
import scipy.stats

SHOW_TITLES=False

# Font config for LaTeX-style labels
plt.rcParams.update({
    "text.usetex": True,
    "font.family": "serif",
    "font.size": 12.5,
})

# Constants
PEAK_BASE = 69         # Subtracted from peak memory values (in MB)
PEAK_THRESHOLD = 10    # Minimum required baseline peak after subtraction (in MB)


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
    elif variant == "copyCR":
        l = max(0, min(1, l * 1.3))  # Lighter
    # Convert back to RGB
    r_new, g_new, b_new = colorsys.hls_to_rgb(h, l, s)
    return (r_new, g_new, b_new)

def compute_fallback_baseline(size, methods, alloc_pivot, copied_pivot, mean_alloc_ratio, mean_copied_ratio):
    alloc_vals = alloc_pivot.loc[size, methods]
    copied_vals = copied_pivot.loc[size, methods]

    num = 0.0
    denom = 0.0
    for method in methods:
        a = alloc_vals[method]
        c = copied_vals[method]
        r_alloc = mean_alloc_ratio.get(method, np.nan)
        r_copied = mean_copied_ratio.get(method, np.nan)
        if not np.isnan(a) and not np.isnan(c) and not np.isnan(r_alloc) and not np.isnan(r_copied):
            num += a * r_alloc #+ c * r_copied
            denom += r_alloc**2 #+ r_copied**2
    return num / denom if denom > 0 else np.nan

def draw(df, test_description, methods, method_baseline, title, output_file=None):
    # Start fresh
    plt.close('all')

    # extract forced color info and normalize methods to a normal list of strings
    forced_color_idxs = [method[1] for method in methods if isinstance(method, tuple)]
    remaining_color_idxs = [i for i in range(10) if i not in forced_color_idxs]
    base_to_idx = {method[0].split('.')[0]: method[1] for method in methods if isinstance(method, tuple)}
    methods = [(method[0] if isinstance(method, tuple) else method) for method in methods]

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
    real_peak = peak_raw - PEAK_BASE
    baseline_peak = real_peak[method_baseline]
    trusted_mask = baseline_peak > PEAK_THRESHOLD
    trusted_sizes = baseline_peak.index[trusted_mask]
    untrusted_sizes = baseline_peak.index[~trusted_mask]

    # Compute per-method geometric mean ratios over trusted sizes
    mean_alloc_ratio = {}
    mean_copied_ratio = {}
    for method in methods:
        alloc_ratio = (alloc_pivot[method] / real_peak[method_baseline]).loc[trusted_sizes]
        copied_ratio = (copied_pivot[method] / real_peak[method_baseline]).loc[trusted_sizes]
        alloc_ratio = alloc_ratio[alloc_ratio > 0]
        copied_ratio = copied_ratio[copied_ratio > 0]
        mean_alloc_ratio[method] = scipy.stats.gmean(alloc_ratio) if not alloc_ratio.empty else np.nan
        mean_copied_ratio[method] = scipy.stats.gmean(copied_ratio) if not copied_ratio.empty else np.nan

    # Assign base colors by prefix (e.g. mapL)
    base_names = dedup_list([m.split('.')[0] for m in methods])
    for (remaining_base, remaining_color_idx) in zip([base for base in base_names if base not in base_to_idx], remaining_color_idxs):
        base_to_idx[remaining_base] = remaining_color_idx

    base_cmap = cm.get_cmap('tab10')
    base_colors = {base: base_cmap(base_to_idx[base]) for base in base_names}

    method_colors = {}
    for method in methods:
        parts = method.split('.')
        base = parts[0]
        variant = parts[1] if len(parts) > 1 else ''
        method_colors[method] = adjust_color(base_colors[base], variant)

    # X-axis ticks
    xtick_powers = list(range(10, 26, 3))
    xticks = [2 ** p for p in xtick_powers]
    xtick_labels = [f"$2^{{{p}}}$" for p in xtick_powers]

    # Create plots
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 7.5), sharex=False)

    # --- Time plot (all sizes included) ---
    for method in methods:
        rel_time = time_pivot[method] / time_pivot[method_baseline]
        ax1.plot(time_pivot.index, rel_time, marker='o', color=method_colors[method],
                 label=f"\\texttt{{{method}}}", linewidth=1.5, alpha=1.0)

    ax1.set_xscale("log", base=2)
    ax1.set_xticks(xticks)
    ax1.set_xticklabels(xtick_labels)
    ax1.axhline(1.0, color='gray', linestyle='--', linewidth=1)
    ax1.set_ylabel(f"Relative time in \\textit{{ms}} (vs. \\texttt{{{method_baseline}}})")
    ax1.set_xlabel("Input size (nb. of elements)")
    ax1.set_title("Time (lower is better)")
    ax1.grid(True, which="both", linestyle="--", linewidth=0.5)
    ax1.legend(framealpha=0.5)

    # --- Memory plot ---

    sizes = sorted(set(filtered_df["SizeNum"]))
    for method in methods:
        color = method_colors[method]
        style = {'linewidth': 1.5, 'alpha': 1.0}

        peak_points_x, peak_points_y = [], []
        alloc_points_x, alloc_points_y = [], []
        copied_points_x, copied_points_y = [], []

        for size in sizes:
            if size not in real_peak.index or method not in real_peak.columns:
                continue

            baseline_value = real_peak.at[size, method_baseline]
            use_real_peak = baseline_value > PEAK_THRESHOLD

            if not use_real_peak:
                # Use mean of allocated + copied at this size
                # alloc_vals = alloc_pivot.loc[size, methods]
                # copied_vals = copied_pivot.loc[size, methods]
                # combined_vals = pd.concat([alloc_vals, copied_vals])
                # baseline = (combined_vals).mean()

                # print(f"fallback baseline for {title}/{size}: {baseline} ({combined_vals})")
                baseline = compute_fallback_baseline(
                    size, methods, alloc_pivot, copied_pivot, mean_alloc_ratio, mean_copied_ratio)
                print(f"fallback baseline at {title}/{size}: {baseline:.6f}")
            else:
                baseline = baseline_value

            # Skip this size if baseline is 0 or NaN
            if baseline <= 0 or pd.isna(baseline):
                continue

            # Plot alloc
            alloc_val = alloc_pivot.at[size, method]
            if pd.notna(alloc_val) and alloc_val > 0:
                alloc_points_x.append(size)
                alloc_points_y.append(alloc_val / baseline)

            # Plot copied
            copied_val = copied_pivot.at[size, method]
            if pd.notna(copied_val) and copied_val > 0:
                copied_points_x.append(size)
                copied_points_y.append(copied_val / baseline)

            # Plot peak only if baseline is reliable
            if use_real_peak:
                peak_val = real_peak.at[size, method]
                if pd.notna(peak_val) and peak_val > 0:
                    peak_points_x.append(size)
                    peak_points_y.append(peak_val / baseline)

        # Plot all collected points
        ax2.plot(peak_points_x, peak_points_y, marker='o', linestyle='-', color=color,
                 label=f"\\texttt{{{method}}} peak", **style)
        ax2.plot(alloc_points_x, alloc_points_y, marker='+', linestyle='--', color=color,
                 label=f"\\texttt{{{method}}} allocated", **style)
        ax2.plot(copied_points_x, copied_points_y, marker='d', linestyle=':', color=color,
                 label=f"\\texttt{{{method}}} copied", **style)

    ax2.set_xscale("log", base=2)
    ax2.set_xticks(xticks)
    ax2.set_xticklabels(xtick_labels)
    ax2.axhline(1.0, color='gray', linestyle='--', linewidth=1)
    ax2.set_ylabel(f"Relative memory usage in \\textit{{MB}} (vs. \\texttt{{{method_baseline}}} peak)")
    ax2.set_xlabel("Input size (nb. of elements)")
    ax2.set_title("Memory (lower is better)")
    ax2.grid(True, which="both", linestyle="--", linewidth=0.5)
    ax2.legend(framealpha=0.5)

    # Super caption
    if SHOW_TITLES:
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
        "relabelPh.force",
        "relabelPh.copyCR",
        "relabelDps",
    ],
    "relabelPh.force",
    "Benchmark of breadth-first tree relabeling",
    "plot-bft.pdf"
)

###############################################################################
#                                    MAP                                      #
###############################################################################
draw(
    df,
    "DPS interface for compact regions/map on List",
    [
        "mapL.force",
        # "mapL.copyCR",
        # "mapS.force",
        # "mapS.copyCR",
        "mapSH.force",
        # "mapSH.copyCR",
        # "mapST.force",
        # "mapST.copyCR",
        # "mapTRL.force",
        # "mapTRL.copyCR",
        "mapTRS.force",
        # "mapTRS.copyCR",
        # "mapTRSH.force",
        # "mapTRSH.copyCR",
        # "mapTRST.force",
        # "mapTRST.copyCR",
        # "mapDpsTRL",
        ("mapDpsTRS", 1),
        # "mapDpsFoldL",
        # "mapDpsFoldLS",
        # "mapDpsFoldSL",
        ("mapDpsFoldS", 3),
    ],
    "mapL.force",
    "Benchmark of map function on list",
    "plot-map.pdf"
)

###############################################################################
#                                    CONCAT                                   #
###############################################################################
draw(
    df,
    "DPS interface for compact regions/List and DList concatenation",
    [
        "concatListRight.force",
        # "concatListRight.copyCR",
        "concatDListFunLeft.force",
        # "concatDListFunLeft.copyCR",
        ("concatDListDpsLeft", 1),
    ],
    "concatListRight.force",
    "Benchmark of iterated concatenations on lists and difference lists",
    "plot-concat.pdf"
)

###############################################################################
#                                    QUEUE                                    #
###############################################################################
draw(
    df,
    "DPS interface for compact regions/Queue enqueue operations",
    [
        "enqueueHMQueue",
        "enqueueEffQueueFun",
        ("enqueueEffQueueDps", 1),
    ],
    "enqueueHMQueue",
    "Benchmark of enqueue operations on queue",
    "plot-queue.pdf"
)

###############################################################################
#                                    PARSER                                   #
###############################################################################
draw(
    df,
    "DPS interface for compact regions/S-expression parser",
    [
        "parseSExpr.force",
        "parseSExpr.copyCR",
        "parseSExprDps",
    ],
    "parseSExpr.force",
    "Benchmark of S-expression parser",
    "plot-parser.pdf"
)
