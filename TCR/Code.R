############################################################ Start | Libraries
library(openxlsx)

library(dplyr)
library(ggplot2)
library(openxlsx)
library(readxl)

############################################################ End | Libraries



############################################################ Start | Reorder essential columns needed for TCR analysis
important_cols <- c(
  "a_cdr3", "b_cdr3", "g_cdr3", "d_cdr3",
  "TRAV", "TRAJ", "TRBV", "TRBJ", "TRGV", "TRGJ", "TRDV", "TRDJ",
  "contigCount_T", "clone_size_ab", "clone_size_gd",
  "imm_receptor", "imm_receptor2", "cluster"
)

important_cols <- important_cols[important_cols %in% colnames(full_metadata)]

remaining_cols <- setdiff(colnames(full_metadata), important_cols)
new_order <- c(important_cols, remaining_cols)

full_metadata <- full_metadata[, new_order]

write.xlsx(full_metadata, "MetaData_CLEANED_reorder_essential_columns_for_TCR.xlsx", row.names = FALSE)

if (exists("patient_colours") & exists("diagnosis_colours") & exists("palette_34")) {
  save(full_metadata, patient_colours, diagnosis_colours, palette_34,
       file = "MetaData_CLEANED_reorder_essential_columns_for_TCR.Rdata")
} else {
  save(full_metadata, file = "MetaData_CLEANED_reorder_essential_columns_for_TCR.Rdata")
}
############################################################ End | Reorder essential columns needed for TCR analysis



############################################################ Start | Clone Size Table
clone_table <- full_metadata %>%
  filter(!is.na(a_cdr3), a_cdr3 != "", !is.na(b_cdr3), b_cdr3 != "") %>%
  count(a_cdr3, b_cdr3, name = "clone_size_ab") %>%
  arrange(desc(clone_size_ab))

colnames(clone_table) <- c("a_cdr3", "b_cdr3", "Clone_Size_ab")

# ذخیره در اکسل
write.xlsx(clone_table, "Clone_Table_with_a_and_b_CDR3.xlsx", row.names = FALSE)
############################################################ End | Clone Size Table



############################################################ Start | Clone Size Count
clone_data <- read_excel("Clone_Table_with_a_and_b_CDR3.xlsx")

clone_size_distribution <- clone_data %>%
  count(Clone_Size_ab, name = "Frequency") %>%
  arrange(desc(Clone_Size_ab))

write.xlsx(clone_size_distribution, "Clone_Size_Count_Table.xlsx", row.names = FALSE)
############################################################ End | Clone Size Count



############################################################ Start | Top10 Clones by Clone Size
clone_data <- read_excel("Clone_Table_with_a_and_b_CDR3.xlsx")

clone_data <- clone_data %>%
  mutate(Clone_ID = paste(a_cdr3, b_cdr3, sep = " + ")) %>%
  filter(!is.na(Clone_Size_ab)) %>%
  arrange(desc(Clone_Size_ab)) %>%
  slice(1:10)  # گرفتن 10 کلون اول

p <- ggplot(clone_data, aes(x = reorder(Clone_ID, -Clone_Size_ab), y = Clone_Size_ab)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Top 10 Clones by Clone Size (ab)",
    x = "Clone ID (a_cdr3 + b_cdr3)",
    y = "Clone Size (ab)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 55, hjust = 1, size = 6),
    axis.text.y = element_text(size = 6),
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 10)
  )

ggsave("Top10_Clones_by_CloneSize.png", plot = p, width = 10, height = 6, dpi = 600, bg = "white")
############################################################ End | Top10 Clones by Clone Size



############################################################ Start | Top20 Clones Cluster Distribution
clone_data <- read_excel("Clone_Table_with_a_and_b_CDR3.xlsx")

clone_data <- clone_data %>%
  mutate(clone_id = paste(a_cdr3, b_cdr3, sep = "_"))

top20_clones <- clone_data %>%
  arrange(desc(Clone_Size_ab)) %>%
  head(20)

full_metadata <- full_metadata %>%
  mutate(clone_id = paste(a_cdr3, b_cdr3, sep = "_"))

clone_cluster_map <- full_metadata %>%
  filter(clone_id %in% top20_clones$clone_id) %>%
  group_by(clone_id) %>%
  summarise(
    Cluster_Count = n_distinct(cluster),
    Clusters = paste(sort(unique(cluster)), collapse = ", ")
  )

final_table <- top20_clones %>%
  select(a_cdr3, b_cdr3, Clone_Size_ab, clone_id) %>%
  left_join(clone_cluster_map, by = "clone_id") %>%
  select(a_cdr3, b_cdr3, Clone_Size_ab, Cluster_Count, Clusters)

write.xlsx(final_table, "Top20_Clones_Cluster_Distribution.xlsx", row.names = FALSE)
############################################################ End | Top20 Clones Cluster Distribution



############################################################ Start | Unique_Clones_Per_Cluster
clone_table <- read_excel("Clone_Table_with_a_and_b_CDR3.xlsx") %>%
  mutate(clone_id = paste(a_cdr3, b_cdr3, sep = "_"))

full_metadata <- full_metadata %>%
  mutate(clone_id = paste(a_cdr3, b_cdr3, sep = "_"))

clone_cluster <- full_metadata %>%
  filter(clone_id %in% clone_table$clone_id) %>%
  select(clone_id, cluster) %>%
  distinct()

cluster_clone_count <- clone_cluster %>%
  group_by(cluster) %>%
  summarise(Unique_Clones = n()) %>%
  arrange(desc(Unique_Clones))

write.xlsx(cluster_clone_count, "Unique_Clones_Per_Cluster.xlsx", row.names = FALSE)


p <- ggplot(cluster_clone_count, aes(x = as.factor(cluster), y = Unique_Clones)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total Number of Unique Clones per Cluster",
    x = "Clusters",
    y = "Number of Unique Clones"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12)
  )

ggsave("Unique_Clones_Per_Cluster_Barplot.png", plot = p,
       width = 10, height = 6, dpi = 600, bg = "white")
############################################################ End | Unique_Clones_Per_Cluster
