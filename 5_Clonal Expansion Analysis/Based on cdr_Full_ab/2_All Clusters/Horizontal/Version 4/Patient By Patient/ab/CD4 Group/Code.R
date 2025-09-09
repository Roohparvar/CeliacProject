library(dplyr)
library(tidyr)
library(ggplot2)
library(scRepertoire)


full_metadata = full_metadata[full_metadata$cluster == "Th" |
                                full_metadata$cluster == "Tregs" |
                                full_metadata$cluster == "Th1 Mem" |
                                full_metadata$cluster == "Th17" , ]


# Filter by Patient if needed
metadata_filtered <- full_metadata %>%
  filter(!is.na(clone_size_ab)) %>%
  mutate(clone_category = case_when(
    clone_size_ab == 1 ~ "Singleton",
    clone_size_ab >= 2 & clone_size_ab <= 10 ~ "Size 2-10",
    clone_size_ab >= 11 & clone_size_ab <= 50 ~ "Size 11-50",
    clone_size_ab >= 51 & clone_size_ab <= 100 ~ "Size 51-100",
    clone_size_ab >= 101 ~ "Size 100+",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(clone_category))


# Summarize by Patient and clone category
df <- metadata_filtered %>%
  group_by(Patient, clone_category) %>%
  summarise(n_cells = n(), .groups = "drop") %>%
  complete(Patient, clone_category = c("Singleton", "Size 2-10", "Size 11-50", "Size 51-100", "Size 100+"),
           fill = list(n_cells = 0)) %>%
  group_by(Patient) %>%
  mutate(perc = if (sum(n_cells) == 0) 0 else n_cells / sum(n_cells) * 100) %>%
  ungroup()


# Order clone categories
df$clone_category <- factor(df$clone_category, levels = rev(c("Singleton", "Size 2-10", "Size 11-50", "Size 51-100", "Size 100+")))

library(writexl)
write_xlsx(df, "df.xlsx")

# Colors and theme
clone_colors <- rev(c("#B3B3B3", "#F0CCFF", "#F8766D", "#619CFF", "#F032E6"))



white_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 2),
    axis.text.y = element_text(size = 2),
    plot.title = element_text(size = 5, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 6),   
    axis.title.y = element_text(size = 6),
    legend.title = element_text(size = 5),
    legend.text = element_text(size = 4),
    legend.key.size = unit(4, "mm")
  )



p1 <- ggplot(df, aes(x = Patient, y = perc, fill = clone_category)) +
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Patient", 
    y = "Percentage of Cells", 
    fill = "Clone Size",
    title = "αβ Clonal Size Distribution by Patient (Percentage) | CD4 Group"
  ) +
  white_theme +
  scale_x_discrete(expand = c(0,10)) +
  coord_cartesian(clip = "off")

ggsave("CD4 Group (Percentage).pdf", p1, width = 4, height = 2, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
ggsave("CD4 Group (Percentage).png", p1, width = 4, height = 2, dpi = 600, bg = "white")




p2 <- ggplot(df, aes(x = Patient, y = n_cells, fill = clone_category)) +
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Patient", 
    y = "Number of Cells", 
    fill = "Clone Size",
    title = "αβ Clonal Size Distribution by Patient (Raw Counts) | CD4 Group"
  ) +
  white_theme +
  scale_x_discrete(expand = c(0,10)) +
  coord_cartesian(clip = "off") 

ggsave("CD4 Group (Raw Counts).pdf", p2, width = 4, height = 2, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
ggsave("CD4 Group (Raw Counts).png", p2, width = 4, height = 2, dpi = 600, bg = "white")
