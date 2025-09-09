library(dplyr)
library(tidyr)
library(ggplot2)

# Filter for ACD diagnosis
full_metadata <- full_metadata[full_metadata$Diagnosis == "ACD", ]

target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)

metadata_filtered <- full_metadata %>%
  filter(!cluster %in% target_clusters)

# Add clone category
metadata_filtered <- metadata_filtered %>%
  mutate(clone_category = case_when(
    clone_size_ab == 1 ~ "Singleton",
    clone_size_ab >= 2 & clone_size_ab <= 10 ~ "Size 2-10",
    clone_size_ab >= 11 & clone_size_ab <= 50 ~ "Size 11-50",
    clone_size_ab >= 51 & clone_size_ab <= 100 ~ "Size 51-100",
    clone_size_ab >= 101 ~ "Size 100+",
    TRUE ~ NA_character_
  ))

# Add a new grouping column based on imm_receptor_Esmaeil
metadata_filtered <- metadata_filtered %>%
  mutate(imm_group = ifelse(imm_receptor_Esmaeil %in% c("Aberrant ab", "Aberrant g"),
                            "Aberrant", "Non-Aberrant"))

# Prepare data for grouped bar chart
df <- metadata_filtered %>%
  filter(!is.na(clone_category)) %>%
  group_by(cluster, imm_group) %>%
  summarise(n_cells = n(), .groups = "drop") %>%
  complete(cluster, imm_group, fill = list(n_cells = 0)) %>%
  ungroup()

df$cluster <- factor(df$cluster)
df$imm_group <- factor(df$imm_group, levels = c("Aberrant", "Non-Aberrant"))

white_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

# Colors for Aberrant vs Non-Aberrant
group_colors <- c("Aberrant" = "#F8766D", "Non-Aberrant" = "#619CFF")

# Create grouped bar chart
p <- ggplot(df, aes(x = cluster, y = n_cells, fill = imm_group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = group_colors) +
  labs(
    x = "Cluster",
    y = "Number of Cells",
    fill = "Imm Receptor",
    title = "Number of Cells by Cluster and Imm Receptor Type"
  ) +
  white_theme

# Save plot
ggsave("GroupedBar_Clusters.png", p, width = 10, height = 5, dpi = 1800, bg = "white")

