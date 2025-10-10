library(dplyr)
library(ggplot2)
library(tidyr)


full_metadata$cluster <- recode(full_metadata$cluster,
                                "Tgd INSIG1+" = "Tγδ INSIG1+",
                                "NK/Tgd" = "NK/Tγδ",
                                "Act. Tgd" = "Act. Tγδ",
                                "Tgd CD8+" = "Tγδ CD8+"
)


target_clusters <- c(
  "Homing plasmablast", "Act. plasma IGHA+", "Mature plasma IGHA+",
  "Act. plasmablast", "Plasma IGHG+", "Mem B cells",
  "B cells BAFFR+", "Macrophages", "pDC", "Mast cells"
)


cluster_summary <- full_metadata %>%
  group_by(cluster) %>%
  summarise(
    Aberrant_ab = sum(imm_receptor_Esmaeil == "Aberrant ab", na.rm = TRUE),
    Aberrant_g = sum(imm_receptor_Esmaeil == "Aberrant g", na.rm = TRUE)
  ) %>%
  mutate(Aberrant_total = Aberrant_ab + Aberrant_g) %>%
  filter(!cluster %in% target_clusters)


# ------------------------------------------------------ Part 1: Aberrant ab vs g  (Version 1) ---------------------------

cluster_long <- cluster_summary %>%
  pivot_longer(
    cols = c(Aberrant_ab, Aberrant_g),
    names_to = "Type",
    values_to = "Count"
  )

png("3_Cluster_Aberrant_Split_Barplot.png", width = 4500, height = 2600, res = 600)

ggplot(cluster_long, aes(x = cluster, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_fill_manual(
    values = c("Aberrant_ab" = "#3a78ce", "Aberrant_g" = "#47ad45"),
    name = "Aberrant Cell Type",
    labels = c("Aberrant_ab" = "Aberrant αβ", "Aberrant_g" = "Aberrant γ")
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


pdf("3_Cluster_Aberrant_Split_Barplot.pdf", width = 12, height = 7)  # width & height in inches

ggplot(cluster_long, aes(x = cluster, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_fill_manual(
    values = c("Aberrant_ab" = "#3a78ce", "Aberrant_g" = "#47ad45"),
    name = "Aberrant Cell Type",
    labels = c("Aberrant_ab" = "Aberrant αβ", "Aberrant_g" = "Aberrant γ")
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
# ------------------------------------------------------ Part 2: Aberrant ab vs g (Version 1) ---------------------------
cluster_long <- cluster_summary %>%
  pivot_longer(
    cols = c(Aberrant_ab, Aberrant_g),
    names_to = "Type",
    values_to = "Count"
  )

png("4_Cluster_Aberrant_Split_Barplot.png", width = 4500, height = 2600, res = 600)

ggplot(cluster_long, aes(x = cluster, y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 0.6) +  # removed position_dodge to stack bars
  scale_fill_manual(
    values = c("Aberrant_ab" = "#3a78ce", "Aberrant_g" = "#47ad45"),
    name = "Aberrant Cell Type",
    labels = c("Aberrant_ab" = "Aberrant αβ", "Aberrant_g" = "Aberrant γ")
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




pdf("4_Cluster_Aberrant_Split_Barplot.pdf", width = 12, height = 7)

ggplot(cluster_long, aes(x = cluster, y = Count, fill = Type)) +
  geom_bar(stat = "identity", width = 0.6) +  # removed position_dodge to stack bars
  scale_fill_manual(
    values = c("Aberrant_ab" = "#3a78ce", "Aberrant_g" = "#47ad45"),
    name = "Aberrant Cell Type",
    labels = c("Aberrant_ab" = "Aberrant αβ", "Aberrant_g" = "Aberrant γ")
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