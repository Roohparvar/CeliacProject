########################################################### Start | Plot UMAP colored by cluster
library(ggplot2)
library(dplyr)

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
########################################################### End | Plot UMAP colored by cluster



########################################################### Start | Duplicate the 'imm_receptor' column as 'imm_receptor2'
full_metadata$imm_receptor2 <- full_metadata$imm_receptor

cols <- colnames(full_metadata)
i <- which(cols == "imm_receptor")

new_order <- append(cols, "imm_receptor2", after = i)
new_order <- new_order[!duplicated(new_order)]

full_metadata <- full_metadata[, new_order]
########################################################### End | Duplicate the 'imm_receptor' column as 'imm_receptor2'



########################################################### Start | Remove "ab", gd", Aberant ab" and "Aberrant g" values from imm_receptor2 in selected B cell clusters
target_clusters <- c(
  "Mast cells", "Plasma cells", "B cells_1", "B cells_2", "B cells MZB1+",
  "Aber. Plasma cells", "Macrophages", "Plasmablast", "B cells BAFFR", "Dendritic cells"
)

target_receptors <- c("Aberant ab", "Aberrant g", "gd", "ab")

rows_to_clean <- which(
  full_metadata$cluster %in% target_clusters &
    full_metadata$imm_receptor2 %in% target_receptors
)
full_metadata$imm_receptor2[rows_to_clean] <- ""
# The number of B cells that were removed: 775
########################################################### End | Remove "gd", Aberant ab" and "Aberrant g" values from imm_receptor2 in selected B cell clusters



########################################################### Start | Remove hkl values from imm_receptor2 in selected T cell clusters
target_clusters <- c(
  "Mast cells", "Plasma cells", "B cells_1", "B cells_2", "B cells MZB1+",
  "Aber. Plasma cells", "Macrophages", "Plasmablast", "B cells BAFFR", "Dendritic cells"
)

rows_to_clear <- which(
  !(full_metadata$cluster %in% target_clusters) & 
    full_metadata$imm_receptor2 == "hkl"
)

length(rows_to_clear)
full_metadata$imm_receptor2[rows_to_clear] <- ""
# The number of T cells that were removed: 1459
########################################################### End | Remove hkl values from imm_receptor2 in selected T cell clusters



########################################################### Start | Clear "T and B" values from imm_receptor2
full_metadata$imm_receptor2[full_metadata$imm_receptor2 == "T and B"] <- ""
# The number of cells with imm_receptor2 == "T and B" that were removed: 1116
########################################################### End | Clear "T and B" values from imm_receptor2



########################################################### Start | Add UMAP plot colored by imm_receptor2 with custom colors
library(ggplot2)

full_metadata$imm_receptor2_clean <- ifelse(
  full_metadata$imm_receptor2 == "" | is.na(full_metadata$imm_receptor2),
  "None",
  full_metadata$imm_receptor2
)


full_metadata$imm_receptor2_clean <- factor(
  full_metadata$imm_receptor2_clean,
  levels = c("None", "ab", "gd", "abgd", "hkl", "Aberant ab", "Aberrant g")
)


custom_colors <- c(
  "ab" = "#1f77b4",
  "gd" = "#ff7f0e",
  "abgd" = "#2ca02c",
  "hkl" = "#d62728",
  "Aberant ab" = "#9467bd",
  "Aberrant g" = "#8c564b",
  "None" = "gray80"
)


png("UMAP_imm_receptor2_layered.png", width = 2000, height = 1600, res = 300)


ggplot(full_metadata, aes(
  x = scVI_with_hvg_UMAP_1,
  y = scVI_with_hvg_UMAP_2
)) +
 
  geom_point(
    data = subset(full_metadata, imm_receptor2_clean == "None"),
    color = "gray80",
    size = 0.8,
    alpha = 0.6
  ) +

  geom_point(
    data = subset(full_metadata, imm_receptor2_clean != "None"),
    aes(color = imm_receptor2_clean),
    size = 0.1,
    alpha = 0.85
  ) +
  scale_color_manual(values = custom_colors, name = "imm_receptor2") +
  labs(
    title = "UMAP colored by imm_receptor2",
    x = "UMAP 1",
    y = "UMAP 2"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  ) +
 
  guides(color = guide_legend(override.aes = list(size = 4)))

dev.off()
########################################################### End | Add UMAP plot colored by imm_receptor2 with custom colors
