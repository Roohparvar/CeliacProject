library(ggplot2)
library(dplyr)
library(scales)

# Desired cluster order
cluster_order <- c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs")
full_metadata$cluster <- factor(full_metadata$cluster, levels = cluster_order)
full_metadata <- full_metadata %>% filter(!is.na(cluster))

cluster_Diagnosis <- c("ACD")

filtered_data <- full_metadata %>%
  filter(!is.na(TRAV) & TRAV != "" & !is.na(TRBV) & TRBV != "", Diagnosis %in% cluster_Diagnosis)


total_counts <- filtered_data %>%
  group_by(cluster) %>%
  summarise(total_cells = n())


selected_data <- filtered_data %>%
  mutate(tcr_group = case_when(
    #TRAV == "TRAV13-1" & TRBV == "TRBV20-1" ~ "TRAV13-1 + TRBV20-1",
    TRAV == "TRAV9-2" ~ "TRAV9-2",
    TRAV == "TRAV13-1" ~ "TRAV13-1",
    TRAV == "TRAV29/DV5" ~ "TRAV29/DV5",
    TRBV == "TRBV5-1" ~ "TRBV5-1",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(tcr_group))


group_counts <- selected_data %>%
  group_by(cluster, tcr_group) %>%
  summarise(count = n(), .groups = "drop")


bar_data <- group_counts %>%
  left_join(total_counts, by = "cluster") %>%
  mutate(freq = count / total_cells * 100)

# تنظیم ترتیب لجند
bar_data$tcr_group <- factor(bar_data$tcr_group, levels = c("TRAV9-2", "TRAV13-1", "TRAV29/DV5", "TRBV5-1"))


tcr_colors <- c(
  #"TRAV13-1 + TRBV20-1" = "#cfe3f6",
  "TRAV9-2"    = "#79A9A9",  # خاکستری متوسط
  "TRAV13-1"   = "#CFE3F6",  # آبی خیلی ملایم
  "TRAV29/DV5" = "#8FAADC",  # آبی متمایل به خاکستری
  "TRBV5-1"    = "#4A708B"   # آبی تیره مایل به خاکستری
)



bar_plot <- ggplot(bar_data, aes(x = cluster, y = freq, fill = tcr_group)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = tcr_colors, name = "") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(x = "", y = "Proportion of CD4+ T cells (%)",
       title = "Proportion of Specific TCR Genes Across CD4 Clusters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))


ggsave(
  "TCR_barplot_by_cluster_selected_groups.png",
  plot = bar_plot,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)
