library(dplyr)
library(ggplot2)
library(tidyr)

# لیست کلاسترهایی که می‌خواهیم حذف کنیم
target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)

# خلاصه‌سازی تعداد سلول‌های aberrant
cluster_summary <- full_metadata %>%
  group_by(cluster) %>%
  summarise(
    Aberrant_ab = sum(imm_receptor_Esmaeil == "Aberrant ab", na.rm = TRUE),
    Aberrant_g = sum(imm_receptor_Esmaeil == "Aberrant g", na.rm = TRUE)
  ) %>%
  mutate(Aberrant_total = Aberrant_ab + Aberrant_g) %>%
  filter(!cluster %in% target_clusters)  # حذف کلاسترهای خاص

# --------- Part 2: Total Aberrant per cluster ---------
png("Cluster_Aberrant_Total_Barplot.png", width = 2000, height = 1400, res = 300)

ggplot(cluster_summary, aes(x = cluster, y = Aberrant_total)) +
  geom_bar(stat = "identity", fill = "purple", width = 0.6) +
  labs(
    title = "Total Aberrant Cells per Cluster",
    x = "Cluster",
    y = "Total Aberrant Cell Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9)
  )

dev.off()

# --------- Part 3: Aberrant ab vs g ---------
# تبدیل به فرمت long
cluster_long <- cluster_summary %>%
  pivot_longer(
    cols = c(Aberrant_ab, Aberrant_g),
    names_to = "Type",
    values_to = "Count"
  )

png("Cluster_Aberrant_Split_Barplot.png", width = 2500, height = 1600, res = 300)

ggplot(cluster_long, aes(x = cluster, y = Count, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  scale_fill_manual(
    values = c("Aberrant_ab" = "#3a78ce", "Aberrant_g" = "#47ad45"),
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
