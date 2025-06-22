# Celiac Project

This repository contains a portion of the data analysis tasks aimed at discovering biologically relevant insights in **Celiac disease**.

---
---
---

## Part 0: Input

The primary input was a **metadata table** with **88 columns**.

---
---
---

## Part 1: Metadata Cleaning

To ensure consistency and reliability of downstream analyses, several metadata cleaning steps were performed:

### Step 1: Corrected Typo in Column Values  
Fixed a typo in the `imm_receptor` column and renamed it to `imm_receptor_Jerome`.

### Step 2: Updated Clusters  
Updated the `cluster` column in `full_metadata` based on `CellID` matches from `updated_clusters.xlsx`.

### Step 3: Combined TCR α and β Chains  
Created `cdr_Full_ab` by combining `a_cdr3` and `b_cdr3` when both were present.

### Step 4: Combined TCR γ and δ Chains  
Created `cdr_Full_gd` by combining `g_cdr3` and `d_cdr3` when both were present.

### Step 5: Combined Ig Heavy and Kappa Chains  
Created `cdr_Full_ig_hk` by combining `h_cdr3` and `k_cdr3` when both were present.

### Step 6: Combined Ig Heavy and Lambda Chains  
Created `cdr_Full_ig_hL` by combining `h_cdr3` and `l_cdr3` when both were present.

### Step 7: Reordered Columns  
Reorganized the metadata columns to a predefined, logical order.

### Step 8: Cleaned B Cell Clusters  
Identified and filtered B cell-related clusters using:

```r
target_clusters <- c(
  "Mast cells", "Plasma cells_1", "B cells_1", "B cells_2", "B cells MZB1+",
  "Plasma cells_2", "Macrophages", "Plasmablast", "B cells BAFFR", "DC"
)
```

Removed rows where `imm_receptor` was one of:
```r
c("Aberrant ab", "Aberrant g", "gd", "ab")
```
Removed B cells: `775`

### Step 9: Cleaned Non-B Cell Clusters (T Cells)  
Removed rows in non-B cell clusters where `imm_receptor == "hkl"`. Removed T cells: `1459`

### Step 10: Removed Ambiguous Immune Receptor Assignments  
Filtered out cells with ambiguous `imm_receptor == "T and B"`. Removed ambiguous cells: `1116`

### Step 11: Removed Immune Receptors from Unexpected Cell Types
Filtered out cells from clusters DC, Macrophages, and Mast cells where imm_receptor was assigned (i.e., not empty or NA).
Removed unexpected immune receptor entries: 71

### Step 12: Removed B Cell Receptor Mismatches
Filtered out cells where either cdr_Full_ig_hk or cdr_Full_ig_hL was present, but imm_receptor was not equal to "hkl".
Removed inconsistent B cell entries: 3438

### Step 13: Updated Patient Identifiers
Patient names were updated based on newly provided metadata. No filtering or removal was performed in this step.

### Step 14: Computed Clone Size for ab TCRs  
Calculated `clone_size_ab` and `clone_size_bucket_ab` based on the frequency of each `cdr_Full_ab` sequence.

### Step 15: Computed Clone Size for gd TCRs  
Calculated `clone_size_gd` and `clone_size_bucket_gd` based on the frequency of each `cdr_Full_gd` sequence.

---
---
---

## Part 2: Distribution of Aberrant T Cells
In this phase, we analyzed how Aberrant T cells are distributed across the different clusters. This step helps to better understand the localization and potential roles of Aberrant T cells within specific cellular populations.

---
---
---

## Part 3: G_CDR3 Combination Analysis
In this section, we explored the distribution of cells that contain the **CDR3_G** sequence and how their combinations with **CDR3_A** and **CDR3_B** are spread across different clusters. This analysis provides insights into the immune receptor pairing patterns and how they may influence cluster-specific behavior or identity.

---
---
---


## Part 4: Immune Receptor Distribution
We analyzed how each immune receptor type (`imm_receptor2`) is distributed across different clusters.  
Bar plots were generated to visualize the counts of each receptor type within clusters.  
This analysis helps characterize the immune landscape and receptor usage across cell populations.

---
---
---

## Part 5: T and B (Ambiguous) Immune Receptor Distribution

In previous steps, we created a new column named imm_receptor_Esmaeil, derived from the original imm_receptor_Jerome, in which we removed cells with ambiguous immune receptor assignments — specifically those labeled as "T and B". While these ambiguous cells were excluded from the final cleaned metadata, this section focuses on analyzing their distribution before removal. The goal is to understand how these "T and B" cells were distributed across clusters and to identify whether they were concentrated in specific regions or broadly spread throughout the dataset.

---
---
---

Part 6: Clonal Homeostasis Plot
In this step, we investigate the distribution of T cell clones based on their expansion levels using the Clonal Homeostasis Plot. This plot provides a visual summary of how TCR clones are distributed by size, categorized into five groups based on their relative abundance:

Rare clones (≤ 0.0001)
Small clones (≤ 0.001)
Medium clones (≤ 0.01)
Large clones (≤ 0.1)
Hyperexpanded clones (> 0.1)

Each bar in the plot represents a cluster, and shows the percentage of TCR repertoire space occupied by clones in each size category.

---
---
---

Part 7: Distribution of Different TCR Combinations
We analyzed immune receptor types across clusters, counted specific TCR types (ab, gd, aberrant forms), and identified unique CDR3 combination patterns. The results were summarized in a structured table and saved to an Excel file.

---
---
---

Part 8: GammaDelta Clone Distribution analysis in BCells
Normally, gamma-delta (γδ) T cell clones are not expected to appear within B cell clusters, so their presence here may indicate clustering or annotation issues that require further investigation. In this part, we analyze the distribution of gamma-delta T cell clones within B cell clusters. The analysis includes visualization of UMAP embeddings highlighting gamma-delta clones and export of relevant metadata for further inspection and cleaning.
---
---
---