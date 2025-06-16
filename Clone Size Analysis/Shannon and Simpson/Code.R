library(dplyr)
library(vegan)
library(tidyr)
library(ggplot2)



calc_diversity <- function(df, group_col) {
  df %>%
    filter(!is.na(cdr_Full_ab), cdr_Full_ab != "") %>%
    group_by(.data[[group_col]]) %>%
    summarise(
      shannon = diversity(table(cdr_Full_ab)),
      simpson = diversity(table(cdr_Full_ab), index = "simpson")
    ) %>%
    rename(Group = .data[[group_col]]) %>%
    filter(Group != "")
}



diversity_cluster <- calc_diversity(full_metadata, "cluster")
diversity_patient <- calc_diversity(full_metadata, "Patient")
diversity_Diagnosis <- calc_diversity(full_metadata, "Diagnosis")



diversity_cluster_long <- diversity_cluster %>% pivot_longer(cols = c(shannon, simpson), names_to = "Index", values_to = "Value")
diversity_patient_long <- diversity_patient %>% pivot_longer(cols = c(shannon, simpson), names_to = "Index", values_to = "Value")
diversity_Diagnosis_long <- diversity_Diagnosis %>% pivot_longer(cols = c(shannon, simpson), names_to = "Index", values_to = "Value")



plot_diversity <- function(data_long, title_text, x_label, axis_text_size = 12) {
  ggplot(data_long, aes(x = reorder(Group, -Value), y = Value, fill = Index)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = title_text,
         x = x_label,
         y = "Diversity Index Value",
         fill = "Index") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = axis_text_size),
      axis.text.y = element_text(size = axis_text_size),
      axis.title.x = element_text(size = axis_text_size + 2),
      axis.title.y = element_text(size = axis_text_size + 2),
      plot.title = element_text(hjust = 0.5, face = "bold", size = axis_text_size + 4)
    )
}




p1 <- plot_diversity(diversity_cluster_long, "Clonal Diversity Metrics per Cluster", "Cluster", axis_text_size = 6)
p2 <- plot_diversity(diversity_patient_long, "Clonal Diversity Metrics per patient", "Patient", axis_text_size = 6)
p3 <- plot_diversity(diversity_Diagnosis_long, "Clonal Diversity Metrics per Diagnosis", "Diagnosis", axis_text_size = 6)



png("Diversity per Cluster.png", width = 1200, height = 800, res = 150)
print(p1)
dev.off()

png("Diversity per Patient.png", width = 1200, height = 800, res = 150)
print(p2)
dev.off()

png("Diversity per Individual Diagnosis.png", width = 1200, height = 800, res = 150)
print(p3)
dev.off()

