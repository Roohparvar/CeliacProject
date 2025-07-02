library(dplyr)
library(ggplot2)
library(tidyr)


target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)


cluster_summary <- full_metadata %>%
  group_by(cluster) %>%
  summarise(
    Aberrant_ab = sum(imm_receptor_Esmaeil == "Aberrant ab", na.rm = TRUE),
    Aberrant_g = sum(imm_receptor_Esmaeil == "Aberrant g", na.rm = TRUE)
  ) %>%
  mutate(Aberrant_total = Aberrant_ab + Aberrant_g) %>%
  filter(!cluster %in% target_clusters)


# ------------------------------------------------------ Part 1: Total Aberrant per cluster ---------------------------
png("1_Cluster_Aberrant_Total_Barplot.png", width = 2000, height = 1400, res = 300)

ggplot(cluster_summary, aes(x = cluster, y = Aberrant_total)) +
  geom_bar(stat = "identity", fill = "#984EA3", width = 0.6) +
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

# ------------------------------------------------------ Part 2: Aberrant ab vs g ---------------------------
cluster_long <- cluster_summary %>%
  pivot_longer(
    cols = c(Aberrant_ab, Aberrant_g),
    names_to = "Type",
    values_to = "Count"
  )

png("2_Cluster_Aberrant_Split_Barplot.png", width = 2500, height = 1600, res = 300)

ggplot(cluster_long, aes(x = cluster, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_fill_manual(
    values = c("Aberrant_ab" = "#3a78ce", "Aberrant_g" = "#47ad45"),
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

# ------------------------------------------------------ Part 3: Aberrant ab vs g ---------------------------

library(ggplot2)
library(dplyr)

color_map <- c(
  "Aberrant ab" = "#3A78CE",
  "Aberrant g"  = "#47AD45",
  "Other"       = "grey80"
)

plot_df <- full_metadata %>%
  mutate(
    receptor_label = case_when(
      imm_receptor_Esmaeil == "Aberrant ab" ~ "Aberrant ab",
      imm_receptor_Esmaeil == "Aberrant g"  ~ "Aberrant g",
      TRUE                                  ~ "Other"
    ),
    receptor_label = factor(receptor_label, levels = c("Other", "Aberrant ab", "Aberrant g"))
  )

umap_plot <- ggplot(plot_df, aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2)) +
  geom_point(data = filter(plot_df, receptor_label == "Other"),
             color = "grey80", size = 0.5, alpha = 0.4) +
  geom_point(data = filter(plot_df, receptor_label != "Other"),
             aes(color = receptor_label), size = 0.2, alpha = 0.8) +
  scale_color_manual(values = color_map, name = "Immune Receptor") +
  guides(color = guide_legend(override.aes = list(size = 3))) + 
  labs(
    title = "UMAP Plot Colored by Aberrant Immune Receptors",
    x = "UMAP 1", y = "UMAP 2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    legend.position = "right"
  )

ggsave("3_UMAP_Aberrant_Receptors.png", umap_plot, width = 8, height = 6, dpi = 400, bg = "white")





# ------------------------------------------------------ Part 4: Aberrant vs Not Aberrant per Cluster plot ---------------------------
library(dplyr)
library(ggplot2)
library(tidyr)

target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)

filtered_df <- full_metadata %>%
  filter(!cluster %in% target_clusters) %>%
  mutate(
    Aberrant_status = ifelse(
      imm_receptor_Esmaeil %in% c("Aberrant ab", "Aberrant g"),
      "Aberrant",
      "Not Aberrant"
    )
  )

count_df <- filtered_df %>%
  group_by(cluster, Aberrant_status) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(cluster, Aberrant_status = c("Aberrant", "Not Aberrant"), fill = list(count = 0))

count_df$cluster <- factor(count_df$cluster, levels = unique(count_df$cluster))

p <- ggplot(count_df, aes(x = cluster, y = count, fill = Aberrant_status)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Number of Aberrant vs Not Aberrant Cells per Cluster (Stacked)",
    x = "Cluster",
    y = "Number of Cells",
    fill = "Aberrant Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# ذخیره نمودار به صورت PNG
ggsave("4_Aberrant_vs_NotAberrant_per_Cluster.png", plot = p, width = 10, height = 6, dpi = 300, bg = "white")
