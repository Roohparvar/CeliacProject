library(dplyr)
library(tidyr)
library(ggplot2)
library(scRepertoire)


full_metadata = full_metadata[full_metadata$cluster == "IEL CCL4+" |
                                full_metadata$cluster == "nIEL" |
                                full_metadata$cluster == "Prolif. IEL" |
                                full_metadata$cluster == "IEL GZMK+" , ]


# Filter by diagnosis if needed
metadata_filtered <- full_metadata %>%
  filter(!is.na(clone_size_gd)) %>%
  mutate(clone_category = case_when(
    clone_size_gd == 1 ~ "Singleton",
    clone_size_gd >= 2 & clone_size_gd <= 10 ~ "Size 2-10",
    clone_size_gd >= 11 & clone_size_gd <= 50 ~ "Size 11-50",
    clone_size_gd >= 51 & clone_size_gd <= 100 ~ "Size 51-100",
    clone_size_gd >= 101 ~ "Size 100+",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(clone_category))


# Summarize by Diagnosis and clone category
df <- metadata_filtered %>%
  group_by(Diagnosis, clone_category) %>%
  summarise(n_cells = n(), .groups = "drop") %>%
  complete(Diagnosis, clone_category = c("Singleton", "Size 2-10", "Size 11-50", "Size 51-100", "Size 100+"),
           fill = list(n_cells = 0)) %>%
  group_by(Diagnosis) %>%
  mutate(perc = if (sum(n_cells) == 0) 0 else n_cells / sum(n_cells) * 100) %>%
  ungroup()


# Order clone categories
df$clone_category <- factor(df$clone_category, levels = rev(c("Singleton", "Size 2-10", "Size 11-50", "Size 51-100", "Size 100+")))


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
    title = "γδ Clonal Size Distribution by Diagnosis (Percentage) | IEL Group"
  ) +
  white_theme +
  scale_x_discrete(expand = c(0,10)) +
  coord_cartesian(clip = "off") 

ggsave("IEL Group (Percentage).pdf", p1, width = 4, height = 2, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
ggsave("IEL Group (Percentage).png", p1, width = 4, height = 2, dpi = 600, bg = "white")





p2 <- ggplot(df, aes(x = Diagnosis, y = n_cells, fill = clone_category)) +
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Diagnosis", 
    y = "Number of Cells", 
    fill = "Clone Size",
    title = "γδ Clonal Size Distribution by Diagnosis (Raw Counts) | IEL Group"
  ) +
  white_theme +
  scale_x_discrete(expand = c(0,10)) +
  coord_cartesian(clip = "off")

ggsave("IEL Group (Raw Counts).pdf", p2, width = 4, height = 2, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
ggsave("IEL Group (Raw Counts).png", p2, width = 4, height = 2, dpi = 600, bg = "white")
