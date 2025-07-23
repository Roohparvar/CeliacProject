library(dplyr)
library(ggplot2)

# Count unique γδ clones per patient
clone_per_patient <- full_metadata %>%
  group_by(Patient) %>%
  summarise(unique_clones = n_distinct(cdr_Full_gd)) %>%
  ungroup() %>%
  arrange(desc(unique_clones)) %>%
  mutate(Patient = factor(Patient, levels = Patient)) 

# Plot
png("Unique_γδ_Clones_Per_Individual_Patient.png", width = 2800, height = 2000, res = 300)
ggplot(clone_per_patient, aes(x = Patient, y = unique_clones)) +
  geom_bar(stat = "identity", fill = "#984ea3") +
  labs(
    title = "Number of Unique γδ Clones per Patient",
    x = "Patient",
    y = "Unique Clones"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7)
  )
dev.off()
