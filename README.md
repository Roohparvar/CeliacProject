# Celiac Project

This repository contains a portion of the data analysis tasks for discovering biologically relevant insights in **Celiac disease**.

---
---
---

## Part 0: Input

The main input was a **metadata table** containing **88 columns**, including sample identifiers, receptor information, clustering results, UMAP coordinates, and other annotations.

The full list of 88 columns:

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

---
---
---

## Part 1: Metadata Cleaning

Due to some technical noise in the metadata, several cleaning steps were performed to ensure data consistency before downstream analyses.

---
### Step 1: Corrected Typo in Column Values.

Fixed a typo in the imm_receptor column values and renamed the column to imm_receptor_Jerome.
---
### Step 2: Updated Clusters.

Updated the cluster column in full_metadata based on matching CellID entries from updated_clusters.xlsx.
---
### Step 3: Combined TCR α and β Chains.

Created a new column cdr_Full_ab by concatenating a_cdr3 and b_cdr3 when both values are present.
---
### Step 4: Combined TCR γ and δ Chains.

Created a new column cdr_Full_gd by concatenating g_cdr3 and d_cdr3 when both values are present.
---
### Step 5: Combined Ig Heavy and Kappa Chains.

Created a new column cdr_Full_ig_hk by concatenating h_cdr3 and k_cdr3 when both values are present.
---
### Step 6: Combined Ig Heavy and Lambda Chains.

Created a new column cdr_Full_ig_hL by concatenating h_cdr3 and l_cdr3 when both values are present.
---
### Step 7: Reordered Columns.

Rearranged columns in the metadata file according to a predefined order for clarity and consistency.
---

### Step 8: Clean B cell clusters

We identified B cell-related clusters using:

```r
target_clusters <- c(
  "Mast cells", "Plasma cells", "B cells_1", "B cells_2", "B cells MZB1+",
  "Aber. Plasma cells", "Macrophages", "Plasmablast", "B cells BAFFR", "Dendritic cells"
)
```

Then, we removed rows where `imm_receptor` was one of:

```r
target_receptors <- c("Aberant ab", "Aberrant g", "gd", "ab")
```

- ✅ **The number of B cells that were removed: 775**

---

### Step 9: Clean non-B cell clusters (T cells)

For clusters not in the above list, we found rows where `imm_receptor == "hkl"` and removed those entries.

- ✅ **The number of T cells that were removed: 1459**

---

### Step 10: Remove ambiguous "T and B" values

Some cells had `imm_receptor == "T and B"` — these were ambiguous and removed as well.

- ✅ **The number of cells with imm_receptor == "T and B" that were removed: 1116**

---
### Step 11: Computed Clone Size for ab Chains.

Populated clone_size_ab and clone_size_bucket_ab columns with the frequency of each cdr_Full_ab sequence.
---
### Step 12: Computed Clone Size for gd Chains.

Populated clone_size_gd and clone_size_bucket_gd columns with the frequency of each cdr_Full_gd sequence.

---
---
---

## Part 2: Immune Receptors Distribution
In this part, we analyzed how each immune receptor type (imm_receptor2) is distributed across different clusters. To visualize this, we generated bar plots showing the count of each immune receptor within each cluster. This analysis helps to better understand the composition of clusters based on immune receptor types and provides insights into their distribution patterns.

---
---
---

## Part 3: TCR Analysis


In this part, we performed T cell receptor (TCR) analysis to investigate the diversity, clonality, and distribution of T cells across clusters. For this analysis, the main columns from the metadata that were required are described below:

1. **PatientName / Patient**: Identifier of the patient/sample; useful for grouping or stratifying TCR results.
2. **a_cdr3, b_cdr3, g_cdr3, d_cdr3, h_cdr3, k_cdr3, l_cdr3**: Complementarity-determining region 3 (CDR3) sequences of various TCR and BCR chains. These are the most informative regions for identifying T cell clones and assessing repertoire diversity.
3. **TRAV, TRBV, TRGV, TRDV**: Variable (V) gene segments for the α, β, γ, and δ chains of TCRs.
4. **TRAJ, TRBJ, TRGJ, TRDJ**: Joining (J) gene segments for the same TCR chains.
5. **contigCount_T**: Number of TCR contigs detected per cell, used as a measure of confidence in TCR identification.
6. **file_T**: Original file from which the TCR data was extracted.
7. **imm_receptor2**: The refined immune receptor assignment used as the main label to distinguish between αβ, γδ, and ambiguous or noisy TCRs.
8. **cluster**: Cell cluster assignment used to relate TCR properties to specific cell populations or phenotypes.
9. **clone_size_ab, clone_size_gd, clone_size_bucket_ab, clone_size_bucket_gd**: Measures of clonal expansion for αβ and γδ TCRs, used in clonality analysis.

These columns allow us to analyze:
1. The distribution of specific TCR clones across cell clusters
2. The gene usage diversity of V and J segments
3. The level of clonal expansion (e.g., in inflamed vs. control samples)
4. The mapping of immune receptor identity (imm_receptor2) to cell types

---
---
---