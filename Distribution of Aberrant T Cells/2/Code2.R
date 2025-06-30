cluster_summary <- full_metadata %>%
  group_by(cluster) %>%
  summarise(
    Aberrant_ab = sum(imm_receptor_Esmaeil_clean == "Aberrant ab", na.rm = TRUE),
    Aberrant_g = sum(imm_receptor_Esmaeil_clean == "Aberrant g", na.rm = TRUE)
  ) %>%
  mutate(
    Aberrant_total = Aberrant_ab + Aberrant_g
  )


# لیست کلاسترهایی که باید حذف شوند
target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)

# حذف کلاسترهای ذکر شده
filtered_cluster_summary <- cluster_summary %>%
  filter(!(cluster %in% target_clusters))

# تبدیل به long format
cluster_long <- filtered_cluster_summary %>%
  pivot_longer(
    cols = c(Aberrant_ab, Aberrant_g),
    names_to = "Type",
    values_to = "Count"
  )

# رسم نمودار
png("Cluster_Aberrant_Split_Barplot.png", width = 2500, height = 1600, res = 300)

ggplot(cluster_long, aes(x = cluster, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_fill_manual(
    values = c("Aberrant_ab" = "red", "Aberrant_g" = "blue"),
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
