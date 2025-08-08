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
    TRAV == "TRAV13-1" & TRBV == "TRBV20-1" ~ "TRAV13-1 + TRBV20-1",
    TRAV == "TRAV9-2" & TRBV == "TRBV20-1" ~ "TRAV9-2 + TRBV20-1",
    TRAV == "TRAV12-2" & TRBV == "TRBV20-1" ~ "TRAV12-2 + TRBV20-1",
    TRAV == "TRAV29/DV5" & TRBV == "TRBV5-1" ~ "TRAV29/DV5 + TRBV5-1",
    TRAV == "TRAV13-1" & TRBV == "TRBV5-1" ~ "TRAV13-1 + TRBV5-1",
    TRAV == "TRAV13-1" & TRBV == "TRBV7-2" ~ "TRAV13-1 + TRBV7-2",
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
  "TRAV13-1 + TRBV20-1" = "#ae1e28",
  "TRAV9-2 + TRBV20-1"      = "#f78454",
  "TRAV12-2 + TRBV20-1"    = "#ffd8c5",
  "TRAV29/DV5 + TRBV5-1"           = "#cfe3f6",
  "TRAV13-1 + TRBV5-1"           = "#5eaada",
  "TRAV13-1 + TRBV7-2"              = "#1367b7"
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
