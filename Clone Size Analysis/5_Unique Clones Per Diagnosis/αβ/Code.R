library(dplyr)
library(ggplot2)


clone_per_id <- full_metadata %>%
  filter(!is.na(Diagnosis) & Diagnosis != "") %>%
  droplevels() %>%
  group_by(Diagnosis) %>%
  summarise(unique_clones = n_distinct(cdr_Full_ab)) %>%
  ungroup() %>%
  arrange(desc(unique_clones)) %>%
  mutate(Diagnosis = factor(Diagnosis, levels = Diagnosis)) 



png("Unique αβ Clones Per Diagnosis.png", width = 2800, height = 2000, res = 300)
ggplot(clone_per_id, aes(x = Diagnosis, y = unique_clones)) +
  geom_bar(stat = "identity", fill = "#984ea3") +
  labs(
    title = "Number of Unique αβ Clones per Diagnosis",
    x = "Diagnosis",
    y = "Unique Clones"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )
dev.off()