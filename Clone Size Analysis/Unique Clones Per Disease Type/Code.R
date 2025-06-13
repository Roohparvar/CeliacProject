library(dplyr)
library(ggplot2)


clone_per_id <- full_metadata %>%
  filter(!is.na(Patient) & Patient != "") %>%
  droplevels() %>%
  group_by(Patient) %>%
  summarise(unique_clones = n_distinct(cdr_Full_ab)) %>%
  ungroup() %>%
  arrange(desc(unique_clones)) %>%
  mutate(Patient = factor(Patient, levels = Patient)) 



png("Unique Clones Per Disease Type.png", width = 2800, height = 2000, res = 300)
ggplot(clone_per_id, aes(x = Patient, y = unique_clones)) +
  geom_bar(stat = "identity", fill = "#984ea3") +
  labs(
    title = "Number of Unique Clones per Disease",
    x = "Disease Type",
    y = "Unique Clones"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )
dev.off()