# Celiac Project

This repository contains a portion of the data analysis tasks aimed at discovering biologically relevant insights in **Celiac disease**.

---
---
---

## Part 0: Input

The primary input was a **metadata table** with **88 columns**, including sample identifiers, immune receptor sequences, clustering results, UMAP coordinates, and various annotations.

<details>
<summary><strong>Click to view the full list of 88 columns</strong></summary>

```
"PatientName"                          "g_cdr3"                               "d_cdr3"
"a_cdr3"                               "b_cdr3"                               "Patient"
"FolderName"                           "Diagnosis"                            "Timepoint"
"Run"                                  "Batch"                                "Tissue"
"Sorted"                               "Type"                                 "Technology"
"FolderNameMulti"                      "HTONameMulti"                         "scanpy_noint_UMAP_1"
"scanpy_noint_UMAP_2"                  "seurat_noint_cluster"                 "rPCA_k3_cluster"
"seurat_noint_UMAP_1"                  "seurat_noint_UMAP_2"                  "rPCA_k3_UMAP_1"
"rPCA_k3_UMAP_2"                       "rPCA_k5_cluster"                      "rPCA_k5_UMAP_1"
"rPCA_k5_UMAP_2"                       "rPCA_k10_cluster"                     "rPCA_k10_UMAP_1"
"rPCA_k10_UMAP_2"                      "rPCA_k20_cluster"                     "rPCA_k20_UMAP_1"
"rPCA_k20_UMAP_2"                      "scVI_no_hvg_leiden_cluster"           "scVI_no_hvg_MDE_1"
"scVI_no_hvg_MDE_2"                    "scVI_no_hvg_UMAP_1"                   "scVI_no_hvg_UMAP_2"
"scVI_with_hvg_leiden_scVI_1"          "scVI_with_hvg_leiden_scVI_1.2"        "scVI_with_hvg_leiden_scVI_1.5"
"scVI_with_hvg_leiden_scVI_1.8"        "scVI_with_hvg_leiden_scVI_2"          "scVI_with_hvg_MDE_1"
"scVI_with_hvg_MDE_2"                  "scVI_with_hvg_UMAP_1"                 "scVI_with_hvg_UMAP_2"
"scVI_with_hvg_paper_leiden_cluster"   "scVI_with_hvg_paper_MDE_1"            "scVI_with_hvg_paper_MDE_2"
"scVI_with_hvg_paper_UMAP_1"           "scVI_with_hvg_paper_UMAP_2"           "nCount_RNA"
"nFeature_RNA"                         "CellID"                               "doubletCluster"
"doubletCell"                          "doublet"                              "mt_ratio"
"scVI_with_hvg_leiden_scVI_1.5_subset" "TRAV"                                 "TRBV"
"TRGV"                                 "TRDV"                                 "TRAJ"
"TRBJ"                                 "TRGJ"                                 "TRDJ"
"contigCount_T"                        "file_T"                               "h_cdr3"
"k_cdr3"                               "l_cdr3"                               "IGHV"
"IGKV"                                 "IGLV"                                 "IGHJ"
"IGKJ"                                 "IGLJ"                                 "contigCount_B"
"file_B"                               "imm_receptor"                         "cluster"
"clone_size_ab"                        "clone_size_gd"                        "clone_size_bucket_gd"
"clone_size_bucket_ab"
```

</details>

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

### Step 11: Computed Clone Size for ab TCRs  
Calculated `clone_size_ab` and `clone_size_bucket_ab` based on the frequency of each `cdr_Full_ab` sequence.

### Step 12: Computed Clone Size for gd TCRs  
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