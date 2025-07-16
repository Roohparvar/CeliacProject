library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)


# List of features to generate plots for
features <- c("cluster", "Diagnosis", "TRAV", "TRAJ", "TRBV", "TRBJ", "TRDV", "TRDJ", "TRGV", "TRGJ")

# Filter cells that are clonally expanded (ab or gd)
expanded_cells <- full_metadata %>%
  filter(
    clone_size_bucket_ab == "Large clone (10+)" | clone_size_bucket_gd == "Large clone (10+)"
  ) %>%
  mutate(
    Expansion_Type = case_when(
      imm_receptor_Esmaeil %in% c("Aberrant ab", "Aberrant g") ~ "Aberrant",
      TRUE ~ "Normal"
    )
  )
# 1 cluster

df_cluster <- expanded_cells %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster, Expansion_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(cluster, Expansion_Type = c("Normal", "Aberrant"), fill = list(count = 0)) %>%
  group_by(cluster) %>%
  mutate(Proportion = count / sum(count) * 100)

# Count plot
ggplot(df_cluster, aes(x = cluster, y = count, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Cell Count by Cluster", x = "Cluster", y = "Cell Count", fill = "Clonally Expanded") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  ggsave("Plot_Count_cluster.png", width = 10, height = 6, dpi = 300, bg = "white")

# Proportion plot
ggplot(df_cluster, aes(x = cluster, y = Proportion, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Cell Types by Cluster", x = "Cluster", y = "Percentage", fill = "Clonally Expanded") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  ggsave("Plot_Proportion_cluster.png", width = 10, height = 6, dpi = 300, bg = "white")
