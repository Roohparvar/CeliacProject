library(dplyr)
library(ggplot2)

# حذف کلاسترهای هدف
target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)

full_metadata <- full_metadata %>%
  filter(!cluster %in% target_clusters)

# فیلتر فقط ab ها
ab_cells <- full_metadata %>%
  filter(imm_receptor_Esmaeil == "gd")

# شمارش تعداد در هر کلاستر
ab_distribution <- ab_cells %>%
  group_by(cluster) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# رسم نمودار
ggplot(ab_distribution, aes(x = reorder(cluster, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Distribution of 'gd' Cells in Clusters",
    x = "Cluster",
    y = "Count of gd Cells"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white")
  )

# اگر بخوای ذخیره هم کنی به صورت PNG با بک‌گراند سفید:
ggsave("ab_cells_distribution.png", width = 10, height = 6, dpi = 300, bg = "white")
