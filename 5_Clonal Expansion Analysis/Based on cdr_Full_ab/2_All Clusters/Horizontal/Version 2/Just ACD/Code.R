if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("scRepertoire")
suppressMessages(library(scRepertoire))

library(dplyr)
library(tidyr)
library(ggplot2)

full_metadata <- full_metadata[full_metadata$Diagnosis == "ACD", ]

target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)
metadata_filtered <- full_metadata %>%
  filter(!cluster %in% target_clusters)


metadata_filtered <- metadata_filtered %>%
  mutate(clone_category = case_when(
    clone_size_ab == 1 ~ "Singleton",
    clone_size_ab >= 2 & clone_size_ab <= 10 ~ "Size 2-10",
    clone_size_ab >= 11 & clone_size_ab <= 50 ~ "Size 11-50",
    clone_size_ab >= 51 & clone_size_ab <= 100 ~ "Size 51-100",
    clone_size_ab >= 101 ~ "Size 100+",
    TRUE ~ NA_character_
  ))


df <- metadata_filtered %>%
  filter(!is.na(clone_category)) %>%
  group_by(cluster, clone_category) %>%
  summarise(n_cells = n(), .groups = "drop") %>%
  complete(cluster, clone_category = c("Singleton", "Size 2-10", "Size 11-50", "Size 51-100", "Size 100+"),
           fill = list(n_cells = 0)) %>%
  group_by(cluster) %>%
  mutate(perc = if (sum(n_cells) == 0) 0 else n_cells / sum(n_cells) * 100) %>%
  ungroup()


df$clone_category <- factor(df$clone_category, levels = rev(c("Singleton", "Size 2-10", "Size 11-50", "Size 51-100", "Size 100+")))
df$cluster <- factor(df$cluster)


white_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )


clone_colors <- rev(c("#1B263B", "#2E4057", "#355C7D", "#3A7BD5", "#1E90FF"))


p1 <- ggplot(df, aes(x = cluster, y = perc, fill = clone_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Cluster", y = "Percentage of Cells", fill = "Clone Size",
    title = "αβ Clonal Size Distribution per Cluster in ACD (Percentage)"
  ) +
  white_theme

ggsave("CloneSize_ab_Percentage.png", p1, width = 10, height = 5, dpi = 400, bg = "white")


p2 <- ggplot(df, aes(x = cluster, y = n_cells, fill = clone_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Cluster", y = "Number of Cells", fill = "Clone Size",
    title = "αβ Clonal Size Distribution per Cluster in ACD (Raw Counts)"
  ) +
  white_theme

ggsave("CloneSize_ab_RawCounts.png", p2, width = 10, height = 5, dpi = 400, bg = "white")
