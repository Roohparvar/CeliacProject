library(dplyr)
library(ggplot2)

# کلاسترهای مورد نظر
cluster_order <- c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs")

# محاسبه درصد بیان TRBV7-2
heatmap_data <- full_metadata %>%
  filter(cluster %in% cluster_order) %>%
  group_by(Diagnosis, cluster) %>%
  summarise(
    percent_TRBV7_2 = mean(TRBV == "TRBV7-2", na.rm = TRUE) * 100,
    count_TRBV7_2 = sum(TRBV == "TRBV7-2", na.rm = TRUE),
    total_cells = n()
  ) %>%
  ungroup()

# ترتیب صحیح فاکتورها
heatmap_data$cluster <- factor(heatmap_data$cluster, levels = cluster_order)

# رسم Heatmap
# رسم Heatmap با عنوان وسط‌چین
p <- ggplot(heatmap_data, aes(x = cluster, y = Diagnosis, fill = percent_TRBV7_2)) +
  geom_tile(color = "white") +
  geom_text(aes(label = paste0(round(percent_TRBV7_2, 1), "%")), size = 3, color = "black") +
  scale_fill_gradient(low = "white", high = "darkred", name = "% TRBV7-2") +
  theme_minimal() +
  labs(
    title = "TRBV7-2 Frequency in CD4+ T Cell Clusters",
    x = "Cluster",
    y = "Diagnosis"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)  # این خط باعث وسط چین شدن عنوان میشه
  )


# ذخیره به PNG
ggsave("TRBV7-2_heatmap.png", plot = p, width = 7, height = 5, dpi = 300, bg = "white")
