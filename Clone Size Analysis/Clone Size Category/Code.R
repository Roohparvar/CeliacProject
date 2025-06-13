library(dplyr)
library(ggplot2)


pie_data <- full_metadata %>%
  filter(!is.na(clone_size_bucket_ab)) %>%
  count(clone_size_bucket_ab) %>%
  mutate(
    percent = round(100 * n / sum(n), 1),
    label = paste0(percent, "%")
  )


png("Clone Size Category.png", width = 2400, height = 2000, res = 300)
ggplot(pie_data, aes(x = "", y = n, fill = clone_size_bucket_ab)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 5) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Clone Size Categories Frequency",
    fill = "Clone Size Category"
  ) +
  theme_void(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 1, face = "bold", size = 18)
  )
dev.off()