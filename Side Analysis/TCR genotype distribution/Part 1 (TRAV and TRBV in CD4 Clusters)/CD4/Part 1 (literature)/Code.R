library(ggplot2)
library(dplyr)
library(scales)

# Desired cluster order
cluster_order <- c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs")
full_metadata$cluster <- factor(full_metadata$cluster, levels = cluster_order)
full_metadata <- full_metadata %>% filter(!is.na(cluster))


filtered_data <- full_metadata %>%
  filter(!is.na(TRAV) & TRAV != "" & !is.na(TRBV) & TRBV != "")


total_counts <- filtered_data %>%
  group_by(cluster) %>%
  summarise(total_cells = n())


selected_data <- filtered_data %>%
  mutate(tcr_group = case_when(
    TRAV == "TRAV26-1" & TRBV == "TRBV7-2" ~ "TRAV26-1 + TRBV7-2",
    TRAV == "TRAV4" & TRBV == "TRBV4" ~ "TRAV4 + TRBV4",
    TRAV == "TRAV4" & TRBV == "TRBV7-2" ~ "TRAV4 + TRBV7-2",
    TRBV == "TRBV20-1" ~ "TRBV20-1",
    TRBV == "TRBV29-1" ~ "TRBV29-1",
    TRBV == "TRBV9" ~ "TRBV9",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(tcr_group))


group_counts <- selected_data %>%
  group_by(cluster, tcr_group) %>%
  summarise(count = n(), .groups = "drop")


bar_data <- group_counts %>%
  left_join(total_counts, by = "cluster") %>%
  mutate(freq = count / total_cells * 100)


tcr_colors <- c(
  "TRAV26-1 + TRBV7-2" = "#ae1e28",
  "TRAV4 + TRBV4"      = "#f78454",
  "TRAV4 + TRBV7-2"    = "#ffd8c5",
  "TRBV20-1"           = "#cfe3f6",
  "TRBV29-1"           = "#5eaada",
  "TRBV9"              = "#1367b7"
)


bar_plot <- ggplot(bar_data, aes(x = cluster, y = freq, fill = tcr_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = tcr_colors, name = "TCR Group") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(x = "Cluster", y = "Percentage of Cells",
       title = "Proportion of Specific TCR Gene Combinations Across CD4 Clusters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave(
  "TCR_barplot_by_cluster_selected_groups.png",
  plot = bar_plot,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)
