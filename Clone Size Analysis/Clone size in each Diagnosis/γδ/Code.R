library(ggplot2)
library(dplyr)

# Define the color map and desired order
color_map <- c(
  "Singleton" = "#999999",
  "Small clone (2+)" = "#FDBF6F",
  "Large clone (10+)" = "#CAB2D6"
)

# Desired order of the clone size categories
clone_order <- c("Large clone (10+)", "Small clone (2+)", "Singleton")

# Prepare summary table, filter, and set factor levels
clone_summary_long <- full_metadata %>%
  filter(clone_size_bucket_gd %in% clone_order) %>%
  mutate(clone_size_bucket_gd = factor(clone_size_bucket_gd, levels = clone_order)) %>%
  group_by(Diagnosis, clone_size_bucket_gd) %>%
  summarise(count = n(), .groups = "drop")

bar_plot <- ggplot(clone_summary_long, aes(x = Diagnosis, y = count, fill = clone_size_bucket_gd)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = color_map, name = "Clone Size", drop = FALSE) +
  theme_minimal(base_size = 14) +   # base size bigger for clarity
  labs(
    title = "TCRγδ Clone Size Distribution by Diagnosis",
    x = "Diagnosis",
    y = "Number of Clones"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),   # center title
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "white", color = NA),  # white panel bg
    plot.background = element_rect(fill = "white", color = NA)    # white plot bg
  )


# Save to PNG
ggsave("clone_distribution_barplot.png", plot = bar_plot, width = 8, height = 5, dpi = 300, bg = "white")



filtered_dataH <- full_metadata %>%
  filter(Diagnosis == "Healthy", clone_size_bucket_gd == "Large clone (10+)")


filtered_dataACD <- full_metadata %>%
  filter(Diagnosis == "ACD", clone_size_bucket_gd == "Large clone (10+)")
