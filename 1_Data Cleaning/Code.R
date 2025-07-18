#--------------------------------------------------------------------------------- libraries
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(Matrix)
library(tidyverse)

#---------------------------------------------------------------------------------  Part 1 - Step 1: Part 1_Step 1: Initial Data Cleaning and Updates 
# Corrected some spelling mistakes in the dataset.
full_metadata$imm_receptor[full_metadata$imm_receptor == "Aberant ab"] <- "Aberrant ab"

# Fixed a typo in the `imm_receptor` column and renamed it to `imm_receptor_Jerome`.
names(full_metadata)[names(full_metadata) == "imm_receptor"] <- "imm_receptor_Jerome"

# A new column `imm_receptor_Esmaeil` was created to preserve the original data before making any further modifications to receptor annotations.
full_metadata$imm_receptor_Esmaeil <- full_metadata$imm_receptor_Jerome

cols <- colnames(full_metadata)
i <- which(cols == "imm_receptor_Jerome")

new_order <- append(cols, "imm_receptor_Esmaeil", after = i)
new_order <- new_order[!duplicated(new_order)]

full_metadata <- full_metadata[, new_order]

# Patient names were updated based on newly provided metadata.
full_metadata = full_metadata %>%
  mutate(Patient = case_when(
    PatientName == "1912" & Timepoint == "T1" ~ "RCD1-1a",
    PatientName == "1912" & Timepoint == "T2" ~ "RCD1-1b",
    PatientName == "1912" & Timepoint == "T3" ~ "RCD1-1c",
    PatientName == "1912" ~ "RCD1-1",
    PatientName == "1996" ~ "RCD1-2",
    PatientName == "6016" ~ "RCD1-3",
    PatientName == "6024" ~ "RCD1-4",
    PatientName == "2025" & Timepoint == "T1" ~ "RCD1-5a",
    PatientName == "2025" & Timepoint == "T2" ~ "RCD1-5b",
    PatientName == "2025" ~ "RCD1-5",
    PatientName == "1960" & Timepoint == "T1" ~ "RCD1-6a",
    PatientName == "1960" & Timepoint == "T2" ~ "RCD1-6b",
    PatientName == "1960" & Timepoint == "NA" ~ "RCD1-6NA",
    PatientName == "1960" ~ "RCD1-6",
    PatientName == "2088" ~ "RCD1-7",
    PatientName == "4562" ~ "RCD1-8",
    PatientName == "6183" ~ "RCD1-9",
    PatientName == "2091" ~ "RCD1-10",
    PatientName == "1935" ~ "RCD1-11",
    PatientName == "1937" ~ "RCD1-12",
    PatientName == "1986" ~ "RCD1-13",
    PatientName == "2020" ~ "RCD1-14",
    PatientName == "1906" ~ "RCD1-15",
    PatientName == "P1" ~ "ACD-1",
    PatientName == "1918" ~ "ACD-2",
    PatientName == "2054" ~ "ACD-3",
    PatientName == "2074" ~ "ACD-4",
    PatientName == "CD10" ~ "ACD-5",
    PatientName == "P2" ~ "ACD-6",
    PatientName == "1670" ~ "ACD-7",
    PatientName == "1872" ~ "ACD-8",
    PatientName == "1902" ~ "ACD-9",
    PatientName == "CD1632" ~ "ACD-10",
    PatientName == "ACD20" ~ "ACD-11",
    PatientName == "8GM" ~ "H-1",
    PatientName == "9HH" ~ "H-2",
    PatientName == "CD11" ~ "H-3",
    PatientName == "CD12" ~ "H-4", 
    PatientName == "CD13" ~ "H-5",
    PatientName == "CD21" ~ "H-6",
    PatientName == "2080" ~ "H-7",
    PatientName == "P5" ~ "H-8",
    PatientName == "P6" ~ "H-9",
    PatientName == "P7" ~ "H-10",
    PatientName == "P8" ~ "H-11",
    PatientName == "P9" ~ "H-12",
    PatientName == "2046" & Timepoint == "T1" ~ "RCD2-1a",
    PatientName == "2046" & Timepoint == "T2" ~ "RCD2-1b",
    PatientName == "2046" ~ "RCD2-1",
    PatientName == "ItalyRCDII" ~ "RCD2-2",
    TRUE ~ NA_character_
  ))

# Update the 'cluster' column in full_metadata based on matching CellID from updated_clusters.xlsx
new_clusters <- read_excel("new_cell_clusters.xlsx")
idx <- match(full_metadata$CellID, new_clusters$CellID)
full_metadata$cluster[!is.na(idx)] <- new_clusters$cluster[idx[!is.na(idx)]]

# Remove rows where imm_receptor_Jerome is "T and B"
removed_count <- sum(full_metadata$imm_receptor_Jerome == "T and B")
full_metadata <- full_metadata[full_metadata$imm_receptor_Jerome != "T and B", ]
# A total of 1116 cells were affected.



#--------------------------------------------------------------------------------- Part 1 - Step 2: Create combined receptor fields when both chains are available:
# Combine 'a_cdr3' and 'b_cdr3' into 'cdr_Full_ab' if both are non-empty
full_metadata <- full_metadata %>%
  mutate(cdr_Full_ab = ifelse(!is.na(a_cdr3) & a_cdr3 != "" &
                                !is.na(b_cdr3) & b_cdr3 != "",
                              paste(a_cdr3, b_cdr3, sep = "+"),
                              NA))

# Combine 'g_cdr3' and 'd_cdr3' into 'cdr_Full_gd' if both are non-empty
full_metadata <- full_metadata %>%
  mutate(cdr_Full_gd = ifelse(!is.na(g_cdr3) & g_cdr3 != "" &
                                !is.na(d_cdr3) & d_cdr3 != "",
                              paste(g_cdr3, d_cdr3, sep = "+"),
                              NA))

# Combine 'h_cdr3' and 'k_cdr3' into 'cdr_Full_ig_hk' if both are non-empty
full_metadata <- full_metadata %>%
  mutate(cdr_Full_ig_hk = ifelse(!is.na(h_cdr3) & h_cdr3 != "" &
                                   !is.na(k_cdr3) & k_cdr3 != "",
                                 paste(h_cdr3, k_cdr3, sep = "+"),
                                 NA))


# Combine 'h_cdr3' and 'l_cdr3' into 'cdr_Full_ig_hL' if both are non-empty
full_metadata <- full_metadata %>%
  mutate(cdr_Full_ig_hL = ifelse(!is.na(h_cdr3) & h_cdr3 != "" &
                                   !is.na(l_cdr3) & l_cdr3 != "",
                                 paste(h_cdr3, l_cdr3, sep = "+"),
                                 NA))

# Reordered columns to follow a standardized and organized structure, including clinical, demographic, scRNA-seq, and TCR-related parameters.
desired_order <- c("PatientName", "Patient", "Diagnosis", "CellID", "cluster",
                   "scVI_with_hvg_UMAP_1", "scVI_with_hvg_UMAP_2", "imm_receptor_Jerome","imm_receptor_Esmaeil",
                   "TRAV", "TRAJ", "a_cdr3", "TRBV", "TRBJ", "b_cdr3", "cdr_Full_ab",
                   "clone_size_ab", "clone_size_bucket_ab", "TRDV", "TRDJ", "d_cdr3",
                   "TRGV", "TRGJ", "g_cdr3", "cdr_Full_gd", "clone_size_gd",
                   "clone_size_bucket_gd", "contigCount_T", "IGHV", "IGHJ", "h_cdr3",
                   "IGKV", "IGKJ", "k_cdr3", "IGLV", "IGLJ", "l_cdr3", "cdr_Full_ig_hk",
                   "cdr_Full_ig_hL", "contigCount_B", "nFeature_RNA", "nCount_RNA",
                   "mt_ratio", "Tissue", "Batch", "Run", "FolderName", "Timepoint",
                   "Sorted", "Type", "Technology", "FolderNameMulti", "HTONameMulti",
                   "scanpy_noint_UMAP_1", "scanpy_noint_UMAP_2", "seurat_noint_cluster",
                   "rPCA_k3_cluster", "seurat_noint_UMAP_1", "seurat_noint_UMAP_2",
                   "rPCA_k3_UMAP_1", "rPCA_k3_UMAP_2", "rPCA_k5_cluster",
                   "rPCA_k5_UMAP_1", "rPCA_k5_UMAP_2", "rPCA_k10_cluster",
                   "rPCA_k10_UMAP_1", "rPCA_k10_UMAP_2", "rPCA_k20_cluster",
                   "rPCA_k20_UMAP_1", "rPCA_k20_UMAP_2", "scVI_no_hvg_leiden_cluster",
                   "scVI_no_hvg_MDE_1", "scVI_no_hvg_MDE_2", "scVI_no_hvg_UMAP_1",
                   "scVI_no_hvg_UMAP_2", "scVI_with_hvg_leiden_scVI_1",
                   "scVI_with_hvg_leiden_scVI_1.2", "scVI_with_hvg_leiden_scVI_1.5",
                   "scVI_with_hvg_leiden_scVI_1.8", "scVI_with_hvg_leiden_scVI_2",
                   "scVI_with_hvg_MDE_1", "scVI_with_hvg_MDE_2",
                   "scVI_with_hvg_paper_leiden_cluster", "scVI_with_hvg_paper_MDE_1",
                   "scVI_with_hvg_paper_MDE_2", "scVI_with_hvg_paper_UMAP_1",
                   "scVI_with_hvg_paper_UMAP_2", "doubletCluster", "doubletCell",
                   "doublet", "scVI_with_hvg_leiden_scVI_1.5_subset", "file_T", "file_B")

full_metadata <- full_metadata %>% select(all_of(desired_order))



#--------------------------------------------------------------------------------- Part 1 - Step 3: Removing T cell receptor information that was assigned to B cell clusters 
# Cleaning imm_receptor_Esmaeil Column
target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR"
)

target_receptors <- c("Aberrant ab", "Aberrant g", "gd", "ab")

rows_to_clean <- which(
  full_metadata$cluster %in% target_clusters &
    full_metadata$imm_receptor_Esmaeil %in% target_receptors
)

if (length(rows_to_clean) > 0) {
  full_metadata$imm_receptor_Esmaeil[rows_to_clean] <- ""
}
# A total of 666 cells were affected.



# Removing Other TCR-Related Data
target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR"
)

rows_to_clean <- which(
  full_metadata$cluster %in% target_clusters
)

if (length(rows_to_clean) > 0) {
  full_metadata$TRAV[rows_to_clean] <- ""
  full_metadata$TRAJ[rows_to_clean] <- ""
  full_metadata$a_cdr3[rows_to_clean] <- ""

  full_metadata$TRBV[rows_to_clean] <- ""
  full_metadata$TRBJ[rows_to_clean] <- ""
  full_metadata$b_cdr3[rows_to_clean] <- ""

  full_metadata$cdr_Full_ab[rows_to_clean] <- ""
  full_metadata$clone_size_ab[rows_to_clean] <- ""
  full_metadata$clone_size_bucket_ab[rows_to_clean] <- ""

  full_metadata$TRDV[rows_to_clean] <- ""
  full_metadata$TRDJ[rows_to_clean] <- ""
  full_metadata$d_cdr3[rows_to_clean] <- ""

  full_metadata$TRGV[rows_to_clean] <- ""
  full_metadata$TRGJ[rows_to_clean] <- ""
  full_metadata$g_cdr3[rows_to_clean] <- ""

  full_metadata$cdr_Full_gd[rows_to_clean] <- ""
  full_metadata$clone_size_gd[rows_to_clean] <- ""
  full_metadata$clone_size_bucket_gd[rows_to_clean] <- ""
}
# A total of 17,792 cells were affected. For these cells, the T Cell Receptor (TCR) data was cleared.



#--------------------------------------------------------------------------------- Part 1 - Step 4: Removing B cell receptor information that was assigned to T cell clusters 
# Cleaning imm_receptor_Esmaeil Column
target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)

rows_to_clean <- which(
  !(full_metadata$cluster %in% target_clusters) & 
    full_metadata$imm_receptor_Esmaeil == "hkl"
)

if (length(rows_to_clean) > 0) {
  full_metadata$imm_receptor_Esmaeil[rows_to_clean] <- ""
}
# A total of 1459 cells were affected. For these cells, the `imm_receptor_Esmaeil` field was cleared.



# Removing Other BCR-Related Data
target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)

rows_to_clean <- which(
  !(full_metadata$cluster %in% target_clusters)
)

if (length(rows_to_clean) > 0) {
  full_metadata$IGHV[rows_to_clean] <- ""
  full_metadata$IGHJ[rows_to_clean] <- ""
  full_metadata$h_cdr3[rows_to_clean] <- ""
  
  full_metadata$IGKV[rows_to_clean] <- ""
  full_metadata$IGKJ[rows_to_clean] <- ""
  full_metadata$k_cdr3[rows_to_clean] <- ""
  
  full_metadata$IGLV[rows_to_clean] <- ""
  full_metadata$IGLJ[rows_to_clean] <- ""
  full_metadata$l_cdr3[rows_to_clean] <- ""
  
  full_metadata$cdr_Full_ig_hk[rows_to_clean] <- ""
  full_metadata$cdr_Full_ig_hL[rows_to_clean] <- ""
}
# A total of 141,029 cells were affected. For these cells, the B Cell Receptor (BCR) data was cleared.



#--------------------------------------------------------------------------------- Part 1 - Step 5: Removing B cell receptor (BCR) and T cell receptor (TCR) information mistakenly assigned to the following clusters: "DC", "Macrophages", and "Mast cells".
target_clusters <- c("DC", "Macrophages", "Mast cells")

rows_to_clean <- which(
  full_metadata$cluster %in% target_clusters & 
    (
      (!is.na(full_metadata$imm_receptor_Esmaeil) & full_metadata$imm_receptor_Esmaeil != "") |
        (!is.na(full_metadata$TRAV) & full_metadata$TRAV != "") |
        (!is.na(full_metadata$TRAJ) & full_metadata$TRAJ != "") |
        (!is.na(full_metadata$a_cdr3) & full_metadata$a_cdr3 != "") |
        (!is.na(full_metadata$TRBV) & full_metadata$TRBV != "") |
        (!is.na(full_metadata$TRBJ) & full_metadata$TRBJ != "") |
        (!is.na(full_metadata$b_cdr3) & full_metadata$b_cdr3 != "") |
        (!is.na(full_metadata$cdr_Full_ab) & full_metadata$cdr_Full_ab != "") |
        (!is.na(full_metadata$clone_size_ab) & full_metadata$clone_size_ab != "") |
        (!is.na(full_metadata$clone_size_bucket_ab) & full_metadata$clone_size_bucket_ab != "") |
        (!is.na(full_metadata$TRDV) & full_metadata$TRDV != "") |
        (!is.na(full_metadata$TRDJ) & full_metadata$TRDJ != "") |
        (!is.na(full_metadata$d_cdr3) & full_metadata$d_cdr3 != "") |
        (!is.na(full_metadata$TRGV) & full_metadata$TRGV != "") |
        (!is.na(full_metadata$TRGJ) & full_metadata$TRGJ != "") |
        (!is.na(full_metadata$g_cdr3) & full_metadata$g_cdr3 != "") |
        (!is.na(full_metadata$cdr_Full_gd) & full_metadata$cdr_Full_gd != "") |
        (!is.na(full_metadata$clone_size_gd) & full_metadata$clone_size_gd != "") |
        (!is.na(full_metadata$clone_size_bucket_gd) & full_metadata$clone_size_bucket_gd != "") |
        (!is.na(full_metadata$IGHV) & full_metadata$IGHV != "") |
        (!is.na(full_metadata$IGHJ) & full_metadata$IGHJ != "") |
        (!is.na(full_metadata$h_cdr3) & full_metadata$h_cdr3 != "") |
        (!is.na(full_metadata$IGKV) & full_metadata$IGKV != "") |
        (!is.na(full_metadata$IGKJ) & full_metadata$IGKJ != "") |
        (!is.na(full_metadata$k_cdr3) & full_metadata$k_cdr3 != "") |
        (!is.na(full_metadata$IGLV) & full_metadata$IGLV != "") |
        (!is.na(full_metadata$IGLJ) & full_metadata$IGLJ != "") |
        (!is.na(full_metadata$l_cdr3) & full_metadata$l_cdr3 != "") |
        (!is.na(full_metadata$cdr_Full_ig_hk) & full_metadata$cdr_Full_ig_hk != "") |
        (!is.na(full_metadata$cdr_Full_ig_hL) & full_metadata$cdr_Full_ig_hL != "")
    )
)


if (length(rows_to_clean) > 0) {
  full_metadata$imm_receptor_Esmaeil[rows_to_clean] <- ""
  
  full_metadata$TRAV[rows_to_clean] <- ""
  full_metadata$TRAJ[rows_to_clean] <- ""
  full_metadata$a_cdr3[rows_to_clean] <- ""
  
  full_metadata$TRBV[rows_to_clean] <- ""
  full_metadata$TRBJ[rows_to_clean] <- ""
  full_metadata$b_cdr3[rows_to_clean] <- ""
  
  full_metadata$cdr_Full_ab[rows_to_clean] <- ""
  full_metadata$clone_size_ab[rows_to_clean] <- ""
  full_metadata$clone_size_bucket_ab[rows_to_clean] <- ""
  
  full_metadata$TRDV[rows_to_clean] <- ""
  full_metadata$TRDJ[rows_to_clean] <- ""
  full_metadata$d_cdr3[rows_to_clean] <- ""
  
  full_metadata$TRGV[rows_to_clean] <- ""
  full_metadata$TRGJ[rows_to_clean] <- ""
  full_metadata$g_cdr3[rows_to_clean] <- ""
  
  full_metadata$cdr_Full_gd[rows_to_clean] <- ""
  full_metadata$clone_size_gd[rows_to_clean] <- ""
  full_metadata$clone_size_bucket_gd[rows_to_clean] <- ""
  
  full_metadata$IGHV[rows_to_clean] <- ""
  full_metadata$IGHJ[rows_to_clean] <- ""
  full_metadata$h_cdr3[rows_to_clean] <- ""
  
  full_metadata$IGKV[rows_to_clean] <- ""
  full_metadata$IGKJ[rows_to_clean] <- ""
  full_metadata$k_cdr3[rows_to_clean] <- ""
  
  full_metadata$IGLV[rows_to_clean] <- ""
  full_metadata$IGLJ[rows_to_clean] <- ""
  full_metadata$l_cdr3[rows_to_clean] <- ""
  
  full_metadata$cdr_Full_ig_hk[rows_to_clean] <- ""
  full_metadata$cdr_Full_ig_hL[rows_to_clean] <- ""
}
# A total of 979 cells were affected.



#--------------------------------------------------------------------------------- Part 1 - Step 6: Update imm_receptor_Esmaeil based on presence of TCR and BCR sequences
# Define non-B cell clusters to check for T cell receptor information
target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)

# Annotate alpha-beta TCR (ab)
rows_ab <- which(
  !(full_metadata$cluster %in% target_clusters) &
    (is.na(full_metadata$imm_receptor_Esmaeil) | full_metadata$imm_receptor_Esmaeil == "") &
    !is.na(full_metadata$cdr_Full_ab) & full_metadata$cdr_Full_ab != "" &
    (is.na(full_metadata$cdr_Full_gd) | full_metadata$cdr_Full_gd == "")
)

if (length(rows_ab) > 0) {
  full_metadata$imm_receptor_Esmaeil[rows_ab] <- "ab"
}
# A total of 5204 cells were affected 


# Annotate gamma-delta TCR (gd)
rows_gd <- which(
  !(full_metadata$cluster %in% target_clusters) &
    (is.na(full_metadata$imm_receptor_Esmaeil) | full_metadata$imm_receptor_Esmaeil == "") &
    !is.na(full_metadata$cdr_Full_gd) & full_metadata$cdr_Full_gd != "" &
    (is.na(full_metadata$cdr_Full_ab) | full_metadata$cdr_Full_ab == "")
)
if (length(rows_gd) > 0) {
full_metadata$imm_receptor_Esmaeil[rows_gd] <- "gd"
}
# A total of 781 cells were affected 


# Annotate both alpha-beta and gamma-delta (abgd)
rows_ab_gd <- which(
  !(full_metadata$cluster %in% target_clusters) &
    (is.na(full_metadata$imm_receptor_Esmaeil) | full_metadata$imm_receptor_Esmaeil == "") &
    !is.na(full_metadata$cdr_Full_ab) & full_metadata$cdr_Full_ab != "" &
    !is.na(full_metadata$cdr_Full_gd) & full_metadata$cdr_Full_gd != ""
)
if (length(rows_ab_gd) > 0) {
full_metadata$imm_receptor_Esmaeil[rows_ab_gd] <- "abgd" # abgd"
}
# A total of 73 cells were affected 



# Annotate BCR sequences (hkl)
target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR"
)

rows_hkl <- which(
  full_metadata$cluster %in% target_clusters &
    (is.na(full_metadata$imm_receptor_Esmaeil) | full_metadata$imm_receptor_Esmaeil == "") &
    (full_metadata$cdr_Full_ig_hk != "" | full_metadata$cdr_Full_ig_hL != "")
)

if (length(rows_hkl) > 0) {
  full_metadata$imm_receptor_Esmaeil[rows_hkl] <- "hkl"
}
# A total of 346 cells were affected 



#--------------------------------------------------------------------------------- Part 1 - Step 7: Managing cells with imm_receptor_Esmaeil annotated as "abgd"
# Update "abgd" to "gd" for cells in Tgd-related clusters
target_clusters <- c("NK Tgd", "Tgd INSIG1+", "Tgd")
rows_to_update_gd <- which(
  full_metadata$cluster %in% target_clusters &
    full_metadata$imm_receptor_Esmaeil == "abgd"
)
full_metadata$imm_receptor_Esmaeil[rows_to_update_gd] <- "gd"
# A total of 113 cells were updated in this step.



# Update "abgd" to "ab" for cells not in Tgd-related clusters
excluded_clusters <- c("NK Tgd", "Tgd INSIG1+", "Tgd", "Tgd CD8+")
rows_to_update_ab <- which(
  !(full_metadata$cluster %in% excluded_clusters) &
    full_metadata$imm_receptor_Esmaeil == "abgd"
)
full_metadata$imm_receptor_Esmaeil[rows_to_update_ab] <- "ab"
# A total of 635 cells were updated in this step.



# Dot plot to identify immune receptor types in Tgd CD8+ cells labeled "abgd"
# Saving all cells where imm_receptor_Esmaeil == "abgd" and cluster == "Tgd CD8+"
abgd_cells <- full_metadata[full_metadata$imm_receptor_Esmaeil == "abgd", ]
write_xlsx(abgd_cells, "1_abgdCells.xlsx")


# Extract main_keys and sub_keys from abgd_cells to analyze the gene expression of these cells
abgd_data <- read_excel("1_abgdCells.xlsx")
main_keys <- abgd_data$FolderName
sub_keys <- gsub("[/-]", ".", abgd_data$CellID)



# Update CellID names by matching with rownames in list_normalised_gene
sub_keys_updated <- sub_keys

# Loop through each main_key (FolderName) and sub_key (CellID)
for (i in seq_along(main_keys)) {
  main_key <- main_keys[i]
  sub_key <- sub_keys[i]
  
  # Check if main_key exists in list_normalised_gene
  if (main_key %in% names(list_normalised_gene)) {
    df <- list_normalised_gene[[main_key]]
    
    rn <- tryCatch(rownames(df), error = function(e) NULL)  # Safely get rownames
    
    if (!is.null(rn)) {
      # If sub_key matches a rowname, keep it
      if (sub_key %in% rn) {
        cat(sprintf("✅ Main key '%s' exists and sub key '%s' found in rownames.\n", main_key, sub_key))
        
      } else {
        # Try prepending 'X' to sub_key (sometimes needed due to name formatting)
        new_sub_key <- paste0("X", sub_key)
        if (new_sub_key %in% rn) {
          cat(sprintf("🔁 Sub key '%s' not found, but '%s' was found. Updating CellID.\n", sub_key, new_sub_key))
          sub_keys_updated[i] <- new_sub_key  # Update to new matching key
        } else {
          cat(sprintf("⚠️ Main key '%s' exists but neither '%s' nor '%s' found in rownames.\n", main_key, sub_key, new_sub_key))
        }
      }
    } else {
      cat(sprintf("❌ Main key '%s' exists but has no rownames.\n", main_key))
    }
    
  } else {
    cat(sprintf("❌ Main key '%s' NOT found in list.\n", main_key))
  }
}

abgd_data$CellID <- sub_keys_updated
write_xlsx(abgd_data, "2_abgdCells_updated.xlsx")





# Extract gene expression of selected markers to prepare for dot plot visualization
abgd_data <- read_excel("2_abgdCells_updated.xlsx")

main_keys <- abgd_data$FolderName
sub_keys <- abgd_data$CellID

target_columns <- tolower(c("Trac", "trbc1", "trbc2", "trgc1", "trgc2", "trdc"))

row_list <- list()

for (i in seq_along(main_keys)) {
  main_key <- main_keys[i]
  sub_key <- sub_keys[i]
  
  if (main_key %in% names(list_normalised_gene)) {
    df <- list_normalised_gene[[main_key]]
    
    rn <- tryCatch(rownames(df), error = function(e) NULL)
    
    if (!is.null(rn) && sub_key %in% rn) {
      row_vector <- as.matrix(df[sub_key, , drop = FALSE])
      
      actual_colnames <- tolower(colnames(row_vector))
      
      row_values <- c()
      
      for (col in target_columns) {
        if (col %in% actual_colnames) {
          match_index <- which(actual_colnames == col)
          row_values[col] <- row_vector[1, match_index]
        } else {
          row_values[col] <- 0
        }
      }
      
      row_values <- c(SubKey = sub_key, row_values)
      row_list[[length(row_list) + 1]] <- row_values
    }
  }
}

final_df <- as.data.frame(do.call(rbind, row_list), stringsAsFactors = FALSE)

cols_to_convert <- setdiff(names(final_df), "SubKey")
final_df[cols_to_convert] <- lapply(final_df[cols_to_convert], as.numeric)

final_df <- final_df[order(-final_df$trac), ]

write_xlsx(final_df, "3_selected_gene_columns_with_subkeys.xlsx")
# Ready to use for dot plot visualization





# Generate a dot plot to visualize the expression levels of key immune receptor genes to help infer the immune receptor type of each cell
df <- read_xlsx("3_selected_gene_columns_with_subkeys.xlsx")
df$SubKey <- factor(df$SubKey, levels = rev(unique(df$SubKey)))

df_long <- df %>%
  pivot_longer(
    cols = -SubKey,
    names_to = "Gene",
    values_to = "Expression"
  )

df_long$Expression <- as.numeric(df_long$Expression)

p <- ggplot(df_long, aes(x = Gene, y = SubKey)) +
  geom_point(aes(size = Expression, color = Expression)) +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(1, 6)) +
  labs(title = "Dot Plot of Gene Expression",
       x = "Gene",
       y = "Cell (SubKey)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

ggsave("Dotplot_gene_expression.pdf", plot = p, width = 8, height = 15, dpi = 300)





# After creating a dot plot for all cells labeled "abgd," we separated alpha-beta, gamma-delta, and ambiguous cells, updated their receptor labels accordingly, and plotted each group separately.
# alpha-beta (ab)
df <- read_xlsx("5_ab.xlsx")

df$SubKey <- factor(df$SubKey, levels = rev(unique(df$SubKey)))

df_long <- df %>%
  pivot_longer(
    cols = -SubKey,
    names_to = "Gene",
    values_to = "Expression"
  )

df_long$Expression <- as.numeric(df_long$Expression)

p <- ggplot(df_long, aes(x = Gene, y = SubKey)) +
  geom_point(aes(size = Expression, color = Expression)) +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(1, 6)) +
  labs(title = "Dot Plot of Gene Expression (ab)",
       x = "Gene",
       y = "Cell (SubKey)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

ggsave("6_alpha_beta_dotplot.pdf", plot = p, width = 8, height = 15, dpi = 300)



target_ids <- c(
  "1906_NA_Gut_Lymph_10x_r2_bNA/TTTGTTGGTTCGGTCG-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/TCATGCCTCTGCCTCA-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/GACCAATGTTCCTACC-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/CACCGTTAGTGAACAT-1",
  "P6_NA_Gut_Lymph_10x_r1_bA/GCGATCGCAATAGGAT-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/AGGAGGTGTGATCGTT-1",
  "P1_NA_Gut_Lymph_10x_r1_bNA/AGAGAGCCAACTTGGT-1",
  "P6_NA_Gut_Lymph_10x_r1_bA/CTCCATGTCTCATGGA-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/CAACAGTAGCAGTACG-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/TTCGATTCAGACAAGC-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/ACAAGCTCAACACTAC-1",
  "P1_NA_Gut_Lymph_10x_r1_bNA/GATTCTTGTCCTCATC-1",
  "P6_NA_Gut_Lymph_10x_r1_bA/CCACAAAAGCCGTTAT-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/TATTGCTCAAGGGTCA-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/AATAGAGAGAAGGATG-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/AATCGTGGTCCAAGAG-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/AGGGTTTGTCTTCCGT-1",
  "P6_NA_Gut_Lymph_10x_r2_bB/AGGTCATCAGGTGTTT-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/AGGACTTAGAGATCGC-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/GTGGTTACACTGCGAC-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/AGGTTACAGATACATG-1",
  "P8_NA_Gut_Lymph_10x_r1_bA/CCGATCTCACTGCATA-1",
  "P6_NA_Gut_Lymph_10x_r1_bA/GTAATCGAGCTCCATA-1",
  "P6_NA_Gut_Lymph_10x_r2_bB/GCACTAATCTTTGGAG-1",
  "P7_NA_Gut_Lymph_10x_r2_bB/GCATCGGGTTAGAGAT-1",
  "P7_NA_Gut_Lymph_10x_r1_bA/AGTGACTCACTGCGAC-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/TTATTGCCAAAGGTTA-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/TGTCCTGGTCCACATA-1",
  "P8_NA_Gut_Lymph_10x_r1_bA/CATGGTACATGGGAAC-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/GAAGTAAGTTAAGGGC-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/CCTAACCAGTGTAGTA-1",
  "P7_NA_Gut_Lymph_10x_r1_bA/ATTGGGTGTGTCTTAG-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/AGTCTCCAGAAGCCTG-1",
  "P7_NA_Gut_Lymph_10x_r1_bA/GGCTTTCCAGCTCTGG-1",
  "1906_NA_Gut_Lymph_10x_r1_bNA/CCGATCTAGAGGCCAT-1",
  "P6_NA_Gut_Lymph_10x_r2_bB/GTTCCGTTCCTGTTGC-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/GTAGTACTCTCCCTAG-1",
  "P6_NA_Gut_Lymph_10x_r1_bA/TGGGTTAAGGAAGAAC-1",
  "P6_NA_Gut_Lymph_10x_r2_bB/TTCCACGAGCCTCAGC-1",
  "P2_NA_Gut_Lymph_10x_r1_bNA/ATCACGAAGTGTTGAA-1",
  "P7_NA_Gut_Lymph_10x_r2_bB/TACCCGTAGTGCACCC-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/CTTCCGATCTATCGCC-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/CTTCCGATCCAAGCAT-1",
  "P7_NA_Gut_Lymph_10x_r2_bB/TTCTGTAAGTCACTAC-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/TACCGGGAGCTAATGA-1",
  "P7_NA_Gut_Lymph_10x_r2_bB/TGAGCGCAGGCGCTCT-1",
  "P7_NA_Gut_Lymph_10x_r2_bB/TCTTCCTTCAGCTGAT-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/GGTCACGTCAAAGGTA-1",
  "P6_NA_Gut_Lymph_10x_r1_bA/GCCAGGTTCCAAGCAT-1",
  "P7_NA_Gut_Lymph_10x_r2_bB/TAGACTGAGGCAGCTA-1",
  "P6_NA_Gut_Lymph_10x_r1_bA/TAACCAGGTGATTAGA-1",
  "P6_NA_Gut_Lymph_10x_r1_bA/GAGTGTTCAGGTTCCG-1",
  "P7_NA_Gut_Lymph_10x_r1_bA/CTCAGAAAGAGTACCG-1",
  "1986_NA_Gut_Lymph_10x_r1_bNA/TTTCATGGTCTCTCCA-1",
  "P6_NA_Gut_Lymph_10x_r2_bB/TACCCGTCACCGCTGA-1",
  "P7_NA_Gut_Lymph_10x_r2_bB/CCGCAAGGTACGTGAG-1",
  "P7_NA_Gut_Lymph_10x_r2_bB/GTGTGGCAGTATGGCG-1",
  "1937_NA_Gut_Lymph_10x_r2_bNA/GGGATGACAAGACCGA-1",
  "P7_NA_Gut_Lymph_10x_r1_bA/CTGCAGGAGCTGTTCA-1",
  "P6_NA_Gut_Lymph_10x_r1_bA/GAAGTAATCCACTGAA-1",
  "P7_NA_Gut_Lymph_10x_r1_bA/CAGCAGCAGGTTTACC-1",
  "P6_NA_Gut_Lymph_10x_r2_bB/TCAGGTATCCGATGCG-1",
  "P9_NA_Gut_Lymph_10x_r2_bB/ATCCCTGAGATTGATG-1",
  "P6_NA_Gut_Lymph_10x_r2_bB/GATAGCTTCGTTCAGA-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/GTGGAGAAGCTCATAC-1",
  "P7_NA_Gut_Lymph_10x_r2_bB/TTCACGCTCTCCGATC-1",
  "P8_NA_Gut_Lymph_10x_r1_bA/TGTTGGAAGAACTGAT-1"
)

full_metadata$imm_receptor_Esmaeil[full_metadata$CellID %in% target_ids] <- "ab"
# 67 cells were confidently labeled as "ab"




# gamma-delta (gd)
df <- read_xlsx("7_gd.xlsx")

df$SubKey <- factor(df$SubKey, levels = rev(unique(df$SubKey)))

df_long <- df %>%
  pivot_longer(
    cols = -SubKey,
    names_to = "Gene",
    values_to = "Expression"
  )

df_long$Expression <- as.numeric(df_long$Expression)

p <- ggplot(df_long, aes(x = Gene, y = SubKey)) +
  geom_point(aes(size = Expression, color = Expression)) +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(1, 6)) +
  labs(title = "Dot Plot of Gene Expression (gd)",
       x = "Gene",
       y = "Cell (SubKey)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

ggsave("8_gamma-delta_dotplot.pdf", plot = p, width = 6, height = 4, dpi = 300)


#Edit immune receptoer
target_ids <- c(
  "1906_NA_Gut_Lymph_10x_r2_bNA/TCCTGCACAGGAATAT-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/GCAACCGCATGTTCGA-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/GGTAGAGTCTCTCCGA-1",
  "P2_NA_Gut_Lymph_10x_r1_bNA/TTACAGGAGTCGGCCT-1",
  "P2_NA_Gut_Lymph_10x_r1_bNA/CATGCAACACGTCGGT-1",
  "P2_NA_Gut_Lymph_10x_r1_bNA/ATCTTCACAGTGTGGA-1"
)

full_metadata$imm_receptor_Esmaeil[full_metadata$CellID %in% target_ids] <- "gd"
# 6 cells were confidently labeled as "gd"


# Ambiguous
df <- read_xlsx("9_Ambiguous.xlsx")

df$SubKey <- factor(df$SubKey, levels = rev(unique(df$SubKey)))

df_long <- df %>%
  pivot_longer(
    cols = -SubKey,
    names_to = "Gene",
    values_to = "Expression"
  )

df_long$Expression <- as.numeric(df_long$Expression)

p <- ggplot(df_long, aes(x = Gene, y = SubKey)) +
  geom_point(aes(size = Expression, color = Expression)) +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(1, 6)) +
  labs(title = "Dot Plot of Gene Expression (Ambiguous)",
       x = "Gene",
       y = "Cell (SubKey)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

ggsave("10_Ambiguous.pdf", plot = p, width = 8, height = 10, dpi = 300)



target_ids <- c(
  "1872_NA_Gut_Lymph_10x_r1_bNA/TACTTACAGCGTTACT-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/ATCACTTAGCGGATCA-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/TGGGAGAAGGTAAACT-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/CAGATTGAGAGCAGCT-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/AGTCTCCCAAGCTGCC-1",
  "P6_NA_Gut_Lymph_10x_r2_bB/AGCTACACAAGCGATG-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/CCAAGCGCAGTTGGTT-1",
  "P6_NA_Gut_Lymph_10x_r1_bA/CTCCCAACATGGAACG-1",
  "1918_NA_Gut_Lymph_10x_r1_bNA/GCGAGAAAGCGGTAGT-1",
  "P2_NA_Gut_Lymph_10x_r1_bNA/TCTTTGATCAAAGAAC-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/TCTGCCATCGCTATTT-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/AACCATGCAACTTGGT-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/TGTGGCGGTGCTGTCG-1",
  "P1_NA_Gut_Lymph_10x_r1_bNA/CCTTTGGCACGCTATA-1",
  "P7_NA_Gut_Lymph_10x_r2_bB/TGGAGAGAGACATATG-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/ACATCGAAGTATGACA-1",
  "1872_NA_Gut_Lymph_10x_r1_bNA/TCCCACACAGACGATG-1",
  "1986_NA_Gut_Lymph_10x_r1_bNA/GCAGCTGAGCGTTAGG-1",
  "1960_T1_Gut_Lymph_10x_r1_bC/CGTTCTGTCTTCCTAA-1",
  "P9_NA_Gut_Lymph_10x_r2_bB/AAAGGGCTCTGTTCAT-1",
  "6016_NA_Gut_Lymph_10x_r1_bNA/TCAGCAAGTATACAGA-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/CTCCACAAGAGCAGTC-1",
  "1986_NA_Gut_Lymph_10x_r1_bNA/CTTGATTTCTAGTTCT-1",
  "P5_NA_Gut_Lymph_10x_r2_bB/TGTGCGGCAGCTTTCC-1",
  "P2_NA_Gut_Lymph_10x_r1_bNA/ATTACCTTCGCTCTAC-1",
  "1906_NA_Gut_Lymph_10x_r2_bNA/AGCGATTCAGGGAGAG-1",
  "P6_NA_Gut_Lymph_10x_r1_bA/CATACTTAGCATGATA-1",
  "P5_NA_Gut_Lymph_10x_r1_bA/GGCTGTGAGTCTCGTA-1",
  "P2_NA_Gut_Lymph_10x_r1_bNA/CAGATACCAGATTTCG-1",
  "P5_NA_Gut_Lymph_10x_r2_bB/AAGTTCGCAACGGCCT-1",
  "P6_NA_Gut_Lymph_10x_r2_bB/GTGTTAGTCACAAGGG-1"
)


full_metadata$imm_receptor_Esmaeil[full_metadata$CellID %in% target_ids] <- ""
# 31 cells remained ambiguous and were labeled as ""


#--------------------------------------------------------------------------------- Part 1 - Step 8: Computing Clone Size for ab and gd TCRs 
# Calculated clone_size_ab and clone_size_bucket_ab based on the frequency of each cdr_Full_ab sequence.
clone_sizes <- table(full_metadata$cdr_Full_ab)
full_metadata$clone_size_ab <- clone_sizes[full_metadata$cdr_Full_ab]


full_metadata$clone_size_bucket_ab <- ifelse(
  is.na(full_metadata$clone_size_ab),
  NA,
  ifelse(
    full_metadata$clone_size_ab == 1,
    "Singleton",
    ifelse(
      full_metadata$clone_size_ab >= 2 & full_metadata$clone_size_ab < 10,
      "Small clone (2+)",
      "Large clone (10+)"
    )
  )
)


# Calculated clone_size_gd and clone_size_bucket_gd based on the frequency of each cdr_Full_gd sequence.
clone_sizes <- table(full_metadata$cdr_Full_gd)
full_metadata$clone_size_gd <- clone_sizes[full_metadata$cdr_Full_gd]


full_metadata$clone_size_bucket_gd <- ifelse(
  is.na(full_metadata$clone_size_gd),
  NA,
  ifelse(
    full_metadata$clone_size_gd == 1,
    "Singleton",
    ifelse(
      full_metadata$clone_size_gd >= 2 & full_metadata$clone_size_gd < 10,
      "Small clone (2+)",
      "Large clone (10+)"
    )
  )
)

#--------------------------------------------------------------------------------- Plot UMAP colored by cluster
full_metadata$cluster <- recode(full_metadata$cluster,
                                "NK Tgd" = "NK Tγδ",
                                "Tgd CD8+" = "Tγδ CD8+",
                                "Tgd INSIG1+" = "Tγδ INSIG1+",
                                "Tgd" = "Tγδ"
)

plot_data <- full_metadata %>%
  filter(!is.na(cluster))  

cluster_centers <- plot_data %>%
  group_by(cluster) %>%
  summarize(x = mean(scVI_with_hvg_UMAP_1),
            y = mean(scVI_with_hvg_UMAP_2))

umap_plot <- ggplot(plot_data, aes(x = scVI_with_hvg_UMAP_1,
                                   y = scVI_with_hvg_UMAP_2,
                                   color = factor(cluster))) +
  geom_point(size = 0.6, alpha = 0.8) +
  geom_text(data = cluster_centers, aes(x = x, y = y, label = cluster),
            color = "black", size = 3, hjust = 0.5, vjust = 0.5) +
  labs(title = "UMAP of Cells Colored by Cluster",
       x = "UMAP 1",
       y = "UMAP 2",
       color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "white", color = NA))

ggsave("UMAP.png", plot = umap_plot, width = 8, height = 6, dpi = 300, bg = "white")



#--------------------------------------------------------------------------------- Save MetaData
save(full_metadata, patient_colours, diagnosis_colours, palette_34, file = "MetaData_Esmaeil.Rdata")