library(dplyr)
library(ggplot2)
library(tidyr)

# ----------------------------------------------------------------------------- Part 1: UMAP plot highlighting Aberrant groups
full_metadata$imm_receptor_Esmaeil_clean <- ifelse(
  full_metadata$imm_receptor_Esmaeil == "" | is.na(full_metadata$imm_receptor_Esmaeil),
  "None",
  full_metadata$imm_receptor_Esmaeil
)

full_metadata$highlight_group <- ifelse(
  full_metadata$imm_receptor_Esmaeil_clean == "Aberrant ab", "Aberrant ab",
  ifelse(full_metadata$imm_receptor_Esmaeil_clean == "Aberrant g", "Aberrant g", NA)
)

png("UMAP_aberrant_with_legend_only.png", width = 2000, height = 1600, res = 300)

ggplot() +
  geom_point(
    data = full_metadata,
    aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2),
    color = "gray80",
    size = 0.1,
    alpha = 0.6
  ) +
  geom_point(
    data = subset(full_metadata, !is.na(highlight_group)),
    aes(
      x = scVI_with_hvg_UMAP_1,
      y = scVI_with_hvg_UMAP_2,
      color = highlight_group
    ),
    size = 0.1,
    alpha = 0.9
  ) +
  scale_color_manual(
    values = c("Aberrant ab" = "red", "Aberrant g" = "blue"),
    name = "imm_receptor"
  ) +
  labs(
    title = "Distribution of Aberrant T Cells",
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



# ----------------------------------------------------------------------------- Part 2 & 3: Summary and Barplots
cluster_summary <- full_metadata %>%
  group_by(cluster) %>%
  summarise(
    Aberrant_ab = sum(imm_receptor_Esmaeil_clean == "Aberrant ab", na.rm = TRUE),
    Aberrant_g = sum(imm_receptor_Esmaeil_clean == "Aberrant g", na.rm = TRUE)
  ) %>%
  mutate(
    Aberrant_total = Aberrant_ab + Aberrant_g
  )

# ----------------------------------------------------------------------------- Part 2: Barplot showing total Aberrant cells per cluster
png("Cluster_Aberrant_Total_Barplot.png", width = 2000, height = 1400, res = 300)

ggplot(cluster_summary, aes(x = cluster, y = Aberrant_total)) +
  geom_bar(stat = "identity", fill = "purple", width = 0.6) +
  labs(
    title = "Total Aberrant Cells per Cluster",
    x = "Cluster",
    y = "Total Aberrant Cell Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9)
  )

dev.off()


# ----------------------------------------------------------------------------- Part 3: Barplot showing Aberrant ab and Aberrant g separately
# Convert to long format for grouped barplot
cluster_long <- cluster_summary %>%
  pivot_longer(
    cols = c(Aberrant_ab, Aberrant_g),
    names_to = "Type",
    values_to = "Count"
  )

png("Cluster_Aberrant_Split_Barplot.png", width = 2500, height = 1600, res = 300)

ggplot(cluster_long, aes(x = cluster, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_fill_manual(
    values = c("Aberrant_ab" = "red", "Aberrant_g" = "blue"),
    name = "Aberrant Cell Type",
    labels = c("Aberrant ab", "Aberrant g")
  ) +
  labs(
    title = "Aberrant Cell Counts per Cluster",
    x = "Cluster",
    y = "Cell Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9)
  )

dev.off()
