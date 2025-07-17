library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

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

# ............................................................................................. 1 cluster
# Prepare data for cluster
df_cluster <- expanded_cells %>%
  filter(!is.na(cluster)) %>%
  group_by(cluster, Expansion_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(cluster, Expansion_Type = c("Normal", "Aberrant"), fill = list(count = 0)) %>%
  group_by(cluster) %>%
  mutate(Proportion = count / sum(count) * 100)

# Plot count by cluster
p_count <- ggplot(df_cluster, aes(x = cluster, y = count, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Cell Count by Cluster",
       x = "Cluster",
       y = "Cell Count",
       fill = "Clonally Expanded") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Count_cluster.png", plot = p_count, width = 10, height = 6, dpi = 300, bg = "white")

# Plot proportion by cluster
p_prop <- ggplot(df_cluster, aes(x = cluster, y = Proportion, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Cell Types by Cluster",
       x = "Cluster",
       y = "Percentage",
       fill = "Clonally Expanded") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Proportion_cluster.png", plot = p_prop, width = 10, height = 6, dpi = 300, bg = "white")











# ............................................................................................. 2 Diagnosis
df_diagnosis <- expanded_cells %>%
  filter(!is.na(Diagnosis)) %>%
  group_by(Diagnosis, Expansion_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(Diagnosis, Expansion_Type = c("Normal", "Aberrant"), fill = list(count = 0)) %>%
  group_by(Diagnosis) %>%
  mutate(Proportion = count / sum(count) * 100)

p_count_diag <- ggplot(df_diagnosis, aes(x = Diagnosis, y = count, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Cell Count by Diagnosis",
       x = "Diagnosis",
       y = "Cell Count",
       fill = "Clonally Expanded") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Count_Diagnosis.png", plot = p_count_diag, width = 10, height = 6, dpi = 300, bg = "white")

p_prop_diag <- ggplot(df_diagnosis, aes(x = Diagnosis, y = Proportion, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Cell Types by Diagnosis",
       x = "Diagnosis",
       y = "Percentage",
       fill = "Clonally Expanded") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Proportion_Diagnosis.png", plot = p_prop_diag, width = 10, height = 6, dpi = 300, bg = "white")





# ............................................................................................. 3 TRAV

df_TRAV <- expanded_cells %>%
  filter(!is.na(TRAV)) %>%
  group_by(TRAV, Expansion_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(TRAV, Expansion_Type = c("Normal", "Aberrant"), fill = list(count = 0)) %>%
  group_by(TRAV) %>%
  mutate(Proportion = count / sum(count) * 100)

p_count_TRAV <- ggplot(df_TRAV, aes(x = TRAV, y = count, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Cell Count by TRAV",
       x = "TRAV",
       y = "Cell Count",
       fill = "Clonally Expanded") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Count_TRAV.png", plot = p_count_TRAV, width = 12, height = 6, dpi = 300, bg = "white")

p_prop_TRAV <- ggplot(df_TRAV, aes(x = TRAV, y = Proportion, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Cell Types by TRAV",
       x = "TRAV",
       y = "Percentage",
       fill = "Clonally Expanded") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Proportion_TRAV.png", plot = p_prop_TRAV, width = 12, height = 6, dpi = 300, bg = "white")




# ............................................................................................. 4 TRAJ

df_TRAJ <- expanded_cells %>%
  filter(!is.na(TRAJ)) %>%
  group_by(TRAJ, Expansion_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(TRAJ, Expansion_Type = c("Normal", "Aberrant"), fill = list(count = 0)) %>%
  group_by(TRAJ) %>%
  mutate(Proportion = count / sum(count) * 100)

p_count_TRAJ <- ggplot(df_TRAJ, aes(x = TRAJ, y = count, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Cell Count by TRAJ",
       x = "TRAJ",
       y = "Cell Count",
       fill = "Clonally Expanded") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Count_TRAJ.png", plot = p_count_TRAJ, width = 12, height = 6, dpi = 300, bg = "white")

p_prop_TRAJ <- ggplot(df_TRAJ, aes(x = TRAJ, y = Proportion, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Cell Types by TRAJ",
       x = "TRAJ",
       y = "Percentage",
       fill = "Clonally Expanded") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Proportion_TRAJ.png", plot = p_prop_TRAJ, width = 12, height = 6, dpi = 300, bg = "white")


# ............................................................................................. 5 TRBV
df_TRBV <- expanded_cells %>%
  filter(!is.na(TRBV)) %>%
  group_by(TRBV, Expansion_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(TRBV, Expansion_Type = c("Normal", "Aberrant"), fill = list(count = 0)) %>%
  group_by(TRBV) %>%
  mutate(Proportion = count / sum(count) * 100)

p_count_TRBV <- ggplot(df_TRBV, aes(x = TRBV, y = count, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Cell Count by TRBV",
       x = "TRBV",
       y = "Cell Count",
       fill = "Clonally Expanded") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Count_TRBV.png", plot = p_count_TRBV, width = 12, height = 6, dpi = 300, bg = "white")

p_prop_TRBV <- ggplot(df_TRBV, aes(x = TRBV, y = Proportion, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Cell Types by TRBV",
       x = "TRBV",
       y = "Percentage",
       fill = "Clonally Expanded") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Proportion_TRBV.png", plot = p_prop_TRBV, width = 12, height = 6, dpi = 300, bg = "white")

# .............................................................................................  6 TRBJ

df_TRBJ <- expanded_cells %>%
  filter(!is.na(TRBJ)) %>%
  group_by(TRBJ, Expansion_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(TRBJ, Expansion_Type = c("Normal", "Aberrant"), fill = list(count = 0)) %>%
  group_by(TRBJ) %>%
  mutate(Proportion = count / sum(count) * 100)

p_count_TRBJ <- ggplot(df_TRBJ, aes(x = TRBJ, y = count, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Cell Count by TRBJ",
       x = "TRBJ",
       y = "Cell Count",
       fill = "Clonally Expanded") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Count_TRBJ.png", plot = p_count_TRBJ, width = 12, height = 6, dpi = 300, bg = "white")

p_prop_TRBJ <- ggplot(df_TRBJ, aes(x = TRBJ, y = Proportion, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Cell Types by TRBJ",
       x = "TRBJ",
       y = "Percentage",
       fill = "Clonally Expanded") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Proportion_TRBJ.png", plot = p_prop_TRBJ, width = 12, height = 6, dpi = 300, bg = "white")


# ............................................................................................. 7 TRDV

df_TRDV <- expanded_cells %>%
  filter(!is.na(TRDV)) %>%
  group_by(TRDV, Expansion_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(TRDV, Expansion_Type = c("Normal", "Aberrant"), fill = list(count = 0)) %>%
  group_by(TRDV) %>%
  mutate(Proportion = count / sum(count) * 100)

p_count_TRDV <- ggplot(df_TRDV, aes(x = TRDV, y = count, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Cell Count by TRDV",
       x = "TRDV",
       y = "Cell Count",
       fill = "Clonally Expanded") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Count_TRDV.png", plot = p_count_TRDV, width = 12, height = 6, dpi = 300, bg = "white")

p_prop_TRDV <- ggplot(df_TRDV, aes(x = TRDV, y = Proportion, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Cell Types by TRDV",
       x = "TRDV",
       y = "Percentage",
       fill = "Clonally Expanded") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Proportion_TRDV.png", plot = p_prop_TRDV, width = 12, height = 6, dpi = 300, bg = "white")


# ............................................................................................. 8 TRDJ

df_TRDJ <- expanded_cells %>%
  filter(!is.na(TRDJ)) %>%
  group_by(TRDJ, Expansion_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(TRDJ, Expansion_Type = c("Normal", "Aberrant"), fill = list(count = 0)) %>%
  group_by(TRDJ) %>%
  mutate(Proportion = count / sum(count) * 100)

p_count_TRDJ <- ggplot(df_TRDJ, aes(x = TRDJ, y = count, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Cell Count by TRDJ",
       x = "TRDJ",
       y = "Cell Count",
       fill = "Clonally Expanded") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Count_TRDJ.png", plot = p_count_TRDJ, width = 12, height = 6, dpi = 300, bg = "white")

p_prop_TRDJ <- ggplot(df_TRDJ, aes(x = TRDJ, y = Proportion, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Cell Types by TRDJ",
       x = "TRDJ",
       y = "Percentage",
       fill = "Clonally Expanded") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Proportion_TRDJ.png", plot = p_prop_TRDJ, width = 12, height = 6, dpi = 300, bg = "white")


# ............................................................................................. 9 TRGV

df_TRGV <- expanded_cells %>%
  filter(!is.na(TRGV)) %>%
  group_by(TRGV, Expansion_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(TRGV, Expansion_Type = c("Normal", "Aberrant"), fill = list(count = 0)) %>%
  group_by(TRGV) %>%
  mutate(Proportion = count / sum(count) * 100)

p_count_TRGV <- ggplot(df_TRGV, aes(x = TRGV, y = count, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Cell Count by TRGV",
       x = "TRGV",
       y = "Cell Count",
       fill = "Clonally Expanded") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Count_TRGV.png", plot = p_count_TRGV, width = 12, height = 6, dpi = 300, bg = "white")

p_prop_TRGV <- ggplot(df_TRGV, aes(x = TRGV, y = Proportion, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Cell Types by TRGV",
       x = "TRGV",
       y = "Percentage",
       fill = "Clonally Expanded") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Proportion_TRGV.png", plot = p_prop_TRGV, width = 12, height = 6, dpi = 300, bg = "white")

# ............................................................................................. 10 TRGJ

df_TRGJ <- expanded_cells %>%
  filter(!is.na(TRGJ)) %>%
  group_by(TRGJ, Expansion_Type) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(TRGJ, Expansion_Type = c("Normal", "Aberrant"), fill = list(count = 0)) %>%
  group_by(TRGJ) %>%
  mutate(Proportion = count / sum(count) * 100)

p_count_TRGJ <- ggplot(df_TRGJ, aes(x = TRGJ, y = count, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Cell Count by TRGJ",
       x = "TRGJ",
       y = "Cell Count",
       fill = "Clonally Expanded") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Count_TRGJ.png", plot = p_count_TRGJ, width = 12, height = 6, dpi = 300, bg = "white")

p_prop_TRGJ <- ggplot(df_TRGJ, aes(x = TRGJ, y = Proportion, fill = Expansion_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of Cell Types by TRGJ",
       x = "TRGJ",
       y = "Percentage",
       fill = "Clonally Expanded") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
        plot.title = element_text(hjust = 0.5))

ggsave("Plot_Proportion_TRGJ.png", plot = p_prop_TRGJ, width = 12, height = 6, dpi = 300, bg = "white")
