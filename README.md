# Celiac Project

This repository contains a portion of the data analysis tasks for discovering biologically relevant insights in **Celiac disease**.

---

## 📥 Input

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

## 🧹 Part 1: Metadata Cleaning

Due to some technical noise in the metadata, several cleaning steps were performed to ensure data consistency before downstream analyses.
___

### Step 1: Clean B cell clusters

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

___

### Step 2: Clean non-B cell clusters (T cells)

For clusters not in the above list, we found rows where `imm_receptor == "hkl"` due to technical noise, and removed those values.

- ✅ **The number of T cells that were removed: 1459**

___

### Step 3: Remove ambiguous "T and B" values

Some cells had `imm_receptor == "T and B"` — these were ambiguous and removed as well.

- ✅ **The number of cells with imm_receptor == "T and B" that were removed: 1116**

---