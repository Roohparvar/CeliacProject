# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("scRepertoire")
# suppressMessages(library(scRepertoire))

library(dplyr)
library(tidyr)
library(ggplot2)

full_metadata <- full_metadata[full_metadata$Diagnosis == "RCD-I", ]

full_metadata <- full_metadata %>%
  filter(
    is.na(a_cdr3) | a_cdr3 == "",
    is.na(b_cdr3) | b_cdr3 == "",
    is.na(d_cdr3) | d_cdr3 == "",
    !is.na(g_cdr3) & g_cdr3 != ""
  )


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












df <- metadata_filtered %>%
  filter(!is.na(clone_category)) %>%
  group_by(cluster, clone_category) %>%
  summarise(
    All_n_cells = n(),
    n_cells_Aberrant = sum(imm_receptor_Esmaeil %in% c("Aberrant ab")),
    n_cells_NonAberrant = sum(!imm_receptor_Esmaeil %in% c("Aberrant ab")),
    .groups = "drop"
  ) %>%
  complete(
    cluster,
    clone_category = c("Singleton", "Size 2-10", "Size 11-50", "Size 51-100", "Size 100+"),
    fill = list(All_n_cells = 0, n_cells_Aberrant = 0, n_cells_NonAberrant = 0)
  ) %>%
  group_by(cluster) %>%
  mutate(
    All_perc = if (sum(All_n_cells) == 0) 0 else All_n_cells / sum(All_n_cells) * 100,
    perc_Aberrant = if (sum(All_n_cells) == 0) 0 else n_cells_Aberrant / sum(All_n_cells) * 100,
    perc_NonAberrant = if (sum(All_n_cells) == 0) 0 else n_cells_NonAberrant / sum(All_n_cells) * 100
  ) %>%
  ungroup()

# تنظیم ترتیب فاکتورها مثل قبل
df$clone_category <- factor(df$clone_category, 
                            levels = rev(c("Singleton", "Size 2-10", "Size 11-50", "Size 51-100", "Size 100+")))
df$cluster <- factor(df$cluster)
























# رنگ‌ها و تم مثل قبل
white_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

clone_colors <- rev(c("#B3B3B3", "#F0CCFF", "#F8766D", "#619CFF", "#F032E6"))

# ---- 1. درصد Aberrant ----
p_ab_perc <- ggplot(df, aes(x = cluster, y = perc_Aberrant, fill = clone_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Cluster", y = "Percentage of Aberrant Cells", fill = "Clone Size",
    title = "Clone Size Distribution - Aberrant (%)"
  ) +
  white_theme

ggsave("CloneSize_Aberrant_Percentage.png", p_ab_perc, width = 10, height = 5, dpi = 1800, bg = "white")


# ---- 2. درصد NonAberrant ----
p_nonab_perc <- ggplot(df, aes(x = cluster, y = perc_NonAberrant, fill = clone_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Cluster", y = "Percentage of NonAberrant Cells", fill = "Clone Size",
    title = "Clone Size Distribution - NonAberrant (%)"
  ) +
  white_theme

ggsave("CloneSize_NonAberrant_Percentage.png", p_nonab_perc, width = 10, height = 5, dpi = 1800, bg = "white")


# ---- 3. تعداد Aberrant ----
p_ab_count <- ggplot(df, aes(x = cluster, y = n_cells_Aberrant, fill = clone_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Cluster", y = "Number of Aberrant Cells", fill = "Clone Size",
    title = "Clone Size Distribution - Aberrant (Counts)"
  ) +
  white_theme

ggsave("CloneSize_Aberrant_Counts.png", p_ab_count, width = 10, height = 5, dpi = 1800, bg = "white")


# ---- 4. تعداد NonAberrant ----
p_nonab_count <- ggplot(df, aes(x = cluster, y = n_cells_NonAberrant, fill = clone_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Cluster", y = "Number of NonAberrant Cells", fill = "Clone Size",
    title = "Clone Size Distribution - NonAberrant (Counts)"
  ) +
  white_theme

ggsave("CloneSize_NonAberrant_Counts.png", p_nonab_count, width = 10, height = 5, dpi = 1800, bg = "white")
