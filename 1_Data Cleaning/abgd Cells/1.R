TandB = full_metadata[full_metadata$imm_receptor == "T and B", ]

library(dplyr)

ab <- TandB[
  !is.na(TandB$a_cdr3) & TandB$a_cdr3 != "" &
    !is.na(TandB$b_cdr3) & TandB$b_cdr3 != "" &
    (is.na(TandB$g_cdr3) | TandB$g_cdr3 == "") &
    (is.na(TandB$d_cdr3) | TandB$d_cdr3 == ""),
]

gd <- TandB[
  !is.na(TandB$g_cdr3) & TandB$g_cdr3 != "" &
    !is.na(TandB$d_cdr3) & TandB$d_cdr3 != "" &
    (is.na(TandB$a_cdr3) | TandB$a_cdr3 == "") &
    (is.na(TandB$b_cdr3) | TandB$b_cdr3 == ""),
]



# Load required libraries
library(ggplot2)
library(dplyr)
library(patchwork)

# Extract UMAP coordinates
full_metadata$UMAP1 <- full_metadata$scVI_with_hvg_UMAP_1
full_metadata$UMAP2 <- full_metadata$scVI_with_hvg_UMAP_2

# Define cluster groups
tgd_clusters <- c("NK Tγδ", "Tγδ INSIG1+", "Tγδ")
tgd_cd8_cluster <- "Tγδ CD8+"

# Additional clusters to exclude in second plot
exclude_clusters <- c(
  tgd_clusters,
  tgd_cd8_cluster,
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)

# Plot 1: Highlight Tγδ clusters in orange
plot1 <- ggplot(full_metadata, aes(x = UMAP1, y = UMAP2)) +
  geom_point(aes(color = ifelse(cluster %in% tgd_clusters, "#FD7D00", "gray")), size = 0.5) +
  scale_color_identity() +
  ggtitle("Tγδ Clusters") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

plot2 <- ggplot(full_metadata, aes(x = UMAP1, y = UMAP2)) +
  geom_point(aes(color = ifelse(!cluster %in% exclude_clusters, "#EE1819", "gray")), size = 0.5) +
  scale_color_identity() +
  ggtitle("Non-Tγδ Clusters") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

plot3 <- ggplot(full_metadata, aes(x = UMAP1, y = UMAP2)) +
  geom_point(aes(color = ifelse(cluster == tgd_cd8_cluster, "#264653", "gray")), size = 0.5) +
  scale_color_identity() +
  ggtitle("Tγδ CD8+ Cluster") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Combine and save the plots
final_plot <- plot1 + plot2 + plot3
ggsave("tgd_umap_panels.png", final_plot, width = 12, height = 4, dpi = 300)
