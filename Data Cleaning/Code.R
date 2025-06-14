########################################################### libraries
library(dplyr)
library(ggplot2)
library(readxl)



###########################################################  Corrects a typo in 'imm_receptor' values and renames the column to 'imm_receptor_Jerome'
full_metadata$imm_receptor[full_metadata$imm_receptor == "Aberant ab"] <- "Aberrant ab"
names(full_metadata)[names(full_metadata) == "imm_receptor"] <- "imm_receptor_Jerome"



########################################################### Update the 'cluster' column in full_metadata based on matching CellID from updated_clusters.xlsx
new_clusters <- read_excel("new_cell_clusters.xlsx")
idx <- match(full_metadata$CellID, new_clusters$CellID)
full_metadata$cluster[!is.na(idx)] <- new_clusters$cluster[idx[!is.na(idx)]]



########################################################### Combine 'a_cdr3' and 'b_cdr3' into 'cdr_Full_ab' if both are non-empty
full_metadata <- full_metadata %>%
  mutate(cdr_Full_ab = ifelse(!is.na(a_cdr3) & a_cdr3 != "" &
                                !is.na(b_cdr3) & b_cdr3 != "",
                              paste(a_cdr3, b_cdr3, sep = "+"),
                              NA))



########################################################### Combine 'g_cdr3' and 'd_cdr3' into 'cdr_Full_gd' if both are non-empty
full_metadata <- full_metadata %>%
  mutate(cdr_Full_gd = ifelse(!is.na(g_cdr3) & g_cdr3 != "" &
                                !is.na(d_cdr3) & d_cdr3 != "",
                              paste(g_cdr3, d_cdr3, sep = "+"),
                              NA))



########################################################### Combine 'h_cdr3' and 'k_cdr3' into 'cdr_Full_ig_hk' if both are non-empty
full_metadata <- full_metadata %>%
  mutate(cdr_Full_ig_hk = ifelse(!is.na(h_cdr3) & h_cdr3 != "" &
                                   !is.na(k_cdr3) & k_cdr3 != "",
                                 paste(h_cdr3, k_cdr3, sep = "+"),
                                 NA))



########################################################### Combine 'h_cdr3' and 'l_cdr3' into 'cdr_Full_ig_hL' if both are non-empty
full_metadata <- full_metadata %>%
  mutate(cdr_Full_ig_hL = ifelse(!is.na(h_cdr3) & h_cdr3 != "" &
                                   !is.na(l_cdr3) & l_cdr3 != "",
                                 paste(h_cdr3, l_cdr3, sep = "+"),
                                 NA))



########################################################### Rearranging the columns of a metadata according to the specified order.
desired_order <- c("PatientName", "Patient", "Diagnosis", "CellID", "cluster",
                   "scVI_with_hvg_UMAP_1", "scVI_with_hvg_UMAP_2", "imm_receptor_Jerome",
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



########################################################### Plot UMAP colored by cluster
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



########################################################### Duplicate the 'imm_receptor_Jerome' column as 'imm_receptor_Esmaeil'
full_metadata$imm_receptor_Esmaeil <- full_metadata$imm_receptor_Jerome

cols <- colnames(full_metadata)
i <- which(cols == "imm_receptor_Jerome")

new_order <- append(cols, "imm_receptor_Esmaeil", after = i)
new_order <- new_order[!duplicated(new_order)]

full_metadata <- full_metadata[, new_order]



########################################################### Remove "ab", gd", Aberrant ab" and "Aberrant g" values from imm_receptor_Esmaeil in selected B cell clusters
target_clusters <- c(
  "Mast cells", "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Macrophages", "Plasmablast",
  "B cells BAFFR", "DC"
)

target_receptors <- c("Aberrant ab", "Aberrant g", "gd", "ab")

rows_to_clean <- which(
  full_metadata$cluster %in% target_clusters &
    full_metadata$imm_receptor_Esmaeil %in% target_receptors
)
full_metadata$imm_receptor_Esmaeil[rows_to_clean] <- ""
# The number of B cells that were removed: 775



########################################################### Remove hkl values from imm_receptor_Esmaeil in selected T cell clusters
target_clusters <- c(
  "Mast cells", "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Macrophages", "Plasmablast",
  "B cells BAFFR", "DC"
)

rows_to_clear <- which(
  !(full_metadata$cluster %in% target_clusters) & 
    full_metadata$imm_receptor_Esmaeil == "hkl"
)


full_metadata$imm_receptor_Esmaeil[rows_to_clear] <- ""
# The number of T cells that were removed: 1459



########################################################### Clear "T and B" values from imm_receptor_Esmaeil
full_metadata$imm_receptor_Esmaeil[full_metadata$imm_receptor_Esmaeil == "T and B"] <- ""
# The number of cells with imm_receptor_Esmaeil == "T and B" that were removed: 1116



########################################################### Updated clone_size_ab and clone_size_bucket_ab columns with the frequency of each cdr_Full_ab sequence
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



########################################################### Updated clone_size_gd and clone_size_bucket_gd columns with the frequency of each cdr_Full_gd sequence
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



########################################################### Add UMAP plot colored by imm_receptor_Esmaeil with custom colors
full_metadata$imm_receptor_Esmaeil_clean <- ifelse(
  full_metadata$imm_receptor_Esmaeil == "" | is.na(full_metadata$imm_receptor_Esmaeil),
  "None",
  full_metadata$imm_receptor_Esmaeil
)


full_metadata$imm_receptor_Esmaeil_clean <- factor(
  full_metadata$imm_receptor_Esmaeil_clean,
  levels = c("None", "ab", "gd", "abgd", "hkl", "Aberrant ab", "Aberrant g")
)


custom_colors <- c(
  "ab" = "#1f77b4",
  "gd" = "#ff7f0e",
  "abgd" = "#2ca02c",
  "hkl" = "#d62728",
  "Aberrant ab" = "#9467bd",
  "Aberrant g" = "#8c564b",
  "None" = "gray80"
)


png("UMAP_imm_receptor_layered.png", width = 2000, height = 1600, res = 300)


ggplot(full_metadata, aes(
  x = scVI_with_hvg_UMAP_1,
  y = scVI_with_hvg_UMAP_2
)) +
  
  geom_point(
    data = subset(full_metadata, imm_receptor_Esmaeil_clean == "None"),
    color = "gray80",
    size = 0.8,
    alpha = 0.6
  ) +
  
  geom_point(
    data = subset(full_metadata, imm_receptor_Esmaeil_clean != "None"),
    aes(color = imm_receptor_Esmaeil_clean),
    size = 0.1,
    alpha = 0.85
  ) +
  scale_color_manual(values = custom_colors, name = "imm_receptor") +
  labs(
    title = "UMAP colored by imm_receptor",
    x = "UMAP 1",
    y = "UMAP 2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
  
  guides(color = guide_legend(override.aes = list(size = 4)))

dev.off()



########################################################### Update Patient column
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



########################################################### Save MetaData
save(full_metadata, patient_colours, diagnosis_colours, palette_34, file = "MetaData_Esmaeil.Rdata")