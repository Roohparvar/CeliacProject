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

### Part 1 - Step 1: Initial Data Cleaning and Updates 
- Corrected some spelling mistakes in the dataset.
- Fixed a typo in the `imm_receptor` column and renamed it to `imm_receptor_Jerome`.
- A new column `imm_receptor_Esmaeil` was created to preserve the original data before making any further modifications to receptor annotations.
- Patient names were updated based on newly provided metadata. 
- Updated the `cluster` column in `full_metadata` based on `CellID` matches from `updated_clusters.xlsx`.
- Removed ambiguous cells where imm_receptor_Jerome was "T and B". **A total of 1116 cells were affected**.

### Part 1 - Step 2: Create combined receptor fields when both chains are available:
- Created `cdr_Full_ab` by combining `a_cdr3` and `b_cdr3` when both were present. 
- Created `cdr_Full_gd` by combining `g_cdr3` and `d_cdr3` when both were present. 
- Created `cdr_Full_ig_hk` by combining `h_cdr3` and `k_cdr3` when both were present. 
- Created `cdr_Full_ig_hL` by combining `h_cdr3` and `l_cdr3` when both were present.
- Reordered columns to follow a standardized and organized structure, including clinical, demographic, scRNA-seq, and TCR-related parameters.  

### Part 1 - Step 3: Removing T cell receptor information that was assigned to B cell clusters
- Cleaning imm_receptor_Esmaeil Column: Remove "ab", gd", Aberrant ab" and "Aberrant g" values from imm_receptor_Esmaeil in selected B cell clusters. **A total of 666 cells were affected**.
- Removing Other TCR-Related Data: TCR-related information assigned to B cell clusters was removed. **A total of 17792 cells were affected**. For these cells, all TCR-related data were cleared to eliminate contamination or misannotation due to doublets or mapping artifacts.

### Part 1 - Step 4: Removing B cell receptor information that was assigned to T cell clusters  
- Cleaning imm_receptor_Esmaeil Column: Remove "hkl" values from imm_receptor_Esmaeil in selected T cell clusters. **A total of 1459 cells were affected**.
- Removing Other BCR-Related Data: BCR-related information assigned to T cell clusters was removed. **A total of 141029 cells were affected**. For these cells, all TCR-related data were cleared to eliminate contamination or misannotation due to doublets or mapping artifacts.

### Part 1 - Step 5: Removing BCR and TCR Information Assigned to "DC", "Macrophages", and "Mast cells"
- The imm_receptor_Esmaeil column was cleared for affected cells within these clusters.
- In addition, all BCR- and TCR-related fields were removed from these cells.
**A total of 180 cells were affected in this step**.

### Part 1 - Step 6: Updating imm_receptor_Esmaeil Based on Receptor Sequences
- ab: Assigned to cells where cdr_Full_ab is present → **5204 cells affected**.
- gd: Assigned to cells where cdr_Full_gd is present → **781 cells affected**.
- abgd: Assigned to cells where both cdr_Full_ab and cdr_Full_gd are present → **73 cells affected**.
- hkl: Assigned to B cell clusters where cdr_Full_ig_hk or cdr_Full_ig_hL is present → **346 cells affected**.

### Part 1 - Step 7: Managing cells with imm_receptor_Esmaeil annotated as "abgd"
- Update "abgd" to "gd" for cells in Tgd-related clusters. **A total of 113 cells were updated in this step**.
- Update "abgd" to "ab" for cells not in Tgd-related clusters. **A total of 635 cells were updated in this step**.
- Created a dot plot to visualize expression levels of key immune receptor genes, aiding in the identification of immune receptor types for cells in the Tgd CD8+ cluster with imm_receptor_Esmaeil labeled as "abgd."

Final classification based on gene expression levels:

- **67 cells were confidently labeled as "ab"**.
- **6 cells were confidently labeled as "gd"**.
- **31 cells remained ambiguous and were labeled as ""**.

### Part 1 - Step 8:  Computing Clone Size for ab and gd TCRs 
- Calculated clone_size_ab and clone_size_bucket_ab based on the frequency of each cdr_Full_ab sequence.
- Calculated clone_size_gd and clone_size_bucket_gd based on the frequency of each cdr_Full_gd sequence.

---
---
---

## Part 2: Distribution of Aberrant T Cells
In this phase, we analyzed the distribution of Aberrant T cells across different clusters. This step involved generating four plots:

- Distribution of Aberrant cells per cluster
- Distribution of Aberrant αβ and γδ cells separately per cluster using UMAP
- Distribution of Aberrant αβ and γδ cells separately per cluster using bar plots
- Comparison of Aberrant vs. Not Aberrant cells per cluster

---
---
---

## Part 3: Clonal Homeostasis Plot
In this part, we investigated the clonal homeostasis of both αβ and γδ T cells by examining how clone sizes are distributed across various immune clusters. This analysis helps assess the diversity and expansion patterns of T cell clones in the dataset.

For each T cell type (αβ and γδ), clones were grouped into five size categories:
- Singleton (clone size = 1)
- Size 2–10
- Size 11–50
- Size 51–100
- Size 100+

We generated two sets of bar plots for each receptor type:
1. A **percentage-based plot** showing the proportion of each clone size group within clusters.
2. A **raw count plot** displaying the actual number of cells in each clone size group per cluster.

These visualizations help highlight the extent of clonal expansion and the balance of T cell diversity across immune environments.

---
---
---

## Part 4: Immune Receptor Distribution
We analyzed how each immune receptor type is distributed across different clusters.
- In the first part of this analysis, we visualized the distribution using a UMAP plot, where cells are colored by their assigned immune receptor types. This provided a global view of receptor-specific localization patterns across the cellular landscape.
- To complement this, we generated bar plots showing the counts of each receptor type within clusters. These bar plots offer a more quantitative perspective, helping to characterize the immune landscape and receptor usage across cell populations.

---
---
---

## Part 5: Clone Size and Diversity Analysis
- Pie Chart of Clone Size Categories: Visualizes the frequency of αβ and γδ T cell clones based on size categories.
- Bar Plot of Top 10 Clones by Size: Displays the largest clones with annotation of receptor type (αβ or γδ).
- UMAP Distribution of Top Clones: Shows the spatial distribution of the top 10 αβ and γδ clones in UMAP space.
- Unique Clones per Cluster: Quantifies the number of unique clones across cell clusters to assess clonal diversity.
- Unique Clones per Diagnosis: Compares clone diversity between clinical diagnosis groups.
- Unique Clones per Patient: Evaluates the diversity of TCR repertoires at the individual patient level.
- Alluvial Plot of Shared Clones: Tracks the distribution and sharing of selected αβ and γδ clones between patients, diagnoses, or clusters.

---
---
---

## Part 3: G_CDR3 Combination Analysis
In this section, we explored the distribution of cells that contain the **CDR3_G** sequence and how their combinations with **CDR3_A** and **CDR3_B** are spread across different clusters. This analysis provides insights into the immune receptor pairing patterns and how they may influence cluster-specific behavior or identity.

---
---
---
## Part 5: T and B (Ambiguous) Immune Receptor Distribution

In previous steps, we created a new column named imm_receptor_Esmaeil, derived from the original imm_receptor_Jerome, in which we removed cells with ambiguous immune receptor assignments — specifically those labeled as "T and B". While these ambiguous cells were excluded from the final cleaned metadata, this section focuses on analyzing their distribution before removal. The goal is to understand how these "T and B" cells were distributed across clusters and to identify whether they were concentrated in specific regions or broadly spread throughout the dataset.


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