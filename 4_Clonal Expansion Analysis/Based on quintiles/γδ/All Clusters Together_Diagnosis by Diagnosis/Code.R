setwd("C:/Esmaeil/CeliacProject/CeliacProject/4_Clonal Expansion Analysis/Based on quintiles/γδ/All Clusters Together_Diagnosis by Diagnosis")


library(dplyr)
library(tidyr)
library(ggplot2)
library(scRepertoire)


# Filter by diagnosis if needed
metadata_filtered <- full_metadata %>%
  filter(!is.na(clone_size_gd)) %>%
  mutate(clone_category = case_when(
    clone_size_gd == 1 ~ "Singleton",
    clone_size_gd >= 2 & clone_size_gd <= 4 ~ "Q1(~50): 2-4",
    clone_size_gd >= 5 & clone_size_gd <= 8 ~ "Q1(50~75): 5-8",
    clone_size_gd >= 9 & clone_size_gd <= 33 ~ "Q1(76~90): 9-33",
    clone_size_gd >= 34 ~ "Q1(91~100): 34-75",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(clone_category))


# Summarize by Diagnosis and clone category
df <- metadata_filtered %>%
  group_by(Diagnosis, clone_category) %>%
  summarise(n_cells = n(), .groups = "drop") %>%
  complete(Diagnosis, clone_category = c("Singleton", "Q1(~50): 2-4", "Q1(50~75): 5-8", "Q1(76~90): 9-33", "Q1(91~100): 34-75"),
           fill = list(n_cells = 0)) %>%
  group_by(Diagnosis) %>%
  mutate(perc = if (sum(n_cells) == 0) 0 else n_cells / sum(n_cells) * 100) %>%
  ungroup()


# Order clone categories
df$clone_category <- factor(df$clone_category, levels = rev(c("Singleton", "Q1(~50): 2-4", "Q1(50~75): 5-8", "Q1(76~90): 9-33", "Q1(91~100): 34-75")))


# Colors and theme
clone_colors <- rev(c("#B3B3B3", "#F0CCFF", "#F8766D", "#619CFF", "#F032E6"))



white_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
    axis.text.y = element_text(size = 3),
    plot.title = element_text(size = 5, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 6),   
    axis.title.y = element_text(size = 6),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 4),
    legend.key.size = unit(4, "mm")
  )



p1 <- ggplot(df, aes(x = Diagnosis, y = perc, fill = clone_category)) +
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Diagnosis", 
    y = "Percentage of Cells", 
    fill = "Clone Size",
    title = "γδ Clonal Size Distribution by Diagnosis (Percentage) | All Clusters"
  ) +
  white_theme +
  scale_x_discrete(expand = c(0,10)) +
  coord_cartesian(clip = "off") 

ggsave("All Clusters (Percentage).pdf", p1, width = 4, height = 2, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
ggsave("All Clusters (Percentage).png", p1, width = 4, height = 2, dpi = 600, bg = "white")





p2 <- ggplot(df, aes(x = Diagnosis, y = n_cells, fill = clone_category)) +
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Diagnosis", 
    y = "Number of Cells", 
    fill = "Clone Size",
    title = "γδ Clonal Size Distribution by Diagnosis (Raw Counts) | All Clusters"
  ) +
  white_theme +
  scale_x_discrete(expand = c(0,10)) +
  coord_cartesian(clip = "off")

ggsave("All Clusters (Raw Counts).pdf", p2, width = 4, height = 2, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
ggsave("All Clusters (Raw Counts).png", p2, width = 4, height = 2, dpi = 600, bg = "white")
