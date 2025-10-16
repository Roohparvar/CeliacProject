library(dplyr)
library(ggplot2)
library(forcats)

trbv_data <- full_metadata %>%
  filter(!is.na(TRBV) & trimws(TRBV) != "") %>%
  select(Diagnosis, gene = TRBV) %>%
  mutate(Diagnosis = factor(Diagnosis, levels = c("ACD", "Healthy", "RCD-I", "RCD-II")))

dot_data <- trbv_data %>%
  group_by(Diagnosis, gene) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Diagnosis) %>%
  mutate(freq = count / sum(count)) %>%
  ungroup() %>%
  filter(!is.na(gene) & trimws(gene) != "")


gene_order <- dot_data %>%
  filter(Diagnosis == "ACD") %>%
  arrange(freq) %>%
  pull(gene)

dot_data <- dot_data %>%
  mutate(gene = factor(gene, levels = gene_order))



dot_plot <- ggplot(dot_data, aes(x = Diagnosis, y = gene)) +
  geom_point(aes(size = freq, color = Diagnosis), alpha = 0.8) +
  scale_size_continuous(name = "Frequency") +
  labs(
    title = "TRBV Gene Usage Across Diagnosis Groups",
    x = "Diagnosis",
    y = "TRBV Gene"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = "TRBV_DotPlot_by_Diagnosis.png",
  plot = dot_plot,
  width = 7,
  height = 12,
  dpi = 300,
  bg = "white"
)
