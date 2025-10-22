library(tidyverse)

# ایجاد یک ستون جدید با مقدار Aberrant یا Non-Aberrant
full_metadata <- full_metadata %>%
  mutate(
    Aberrance = case_when(
      imm_receptor_Esmaeil %in% c("Aberrant ab", "Aberrant g") ~ "Aberrant",
      TRUE ~ "Non-Aberrant"
    )
  )

# شمارش تعداد هر نوع در هر کلاستر
cluster_counts <- full_metadata %>%
  group_by(cluster, Aberrance) %>%
  summarise(Count = n()) %>%
  ungroup()

# رسم نمودار
p <- ggplot(cluster_counts, aes(x = cluster, y = Count, fill = Aberrance)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(title = "Aberrant vs Non-Aberrant Cells per Cluster",
       x = "Cluster",
       y = "Cell Count",
       fill = "Cell Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# نمایش نمودار
print(p)

# ذخیره نمودار به صورت PNG
ggsave("aberrant_vs_nonaberrant_per_cluster.png", plot = p, width = 10, height = 6, dpi = 300)
ggsave("aberrant_vs_nonaberrant_per_cluster.png", plot = p, width = 10, height = 6, dpi = 300, bg = "white")
