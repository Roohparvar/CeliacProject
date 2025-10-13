library(dplyr)



full_metadata = full_metadata[full_metadata$cluster == "Plasma cells_1" |
                                full_metadata$cluster == "B cells_1" |
                                full_metadata$cluster == "B cells_2" |
                                full_metadata$cluster == "B cells MZB1+" |
                                full_metadata$cluster == "Plasma cells_2" |
                                full_metadata$cluster == "Plasmablast" |
                                full_metadata$cluster == "B cells BAFFR" , ]


full_metadata <- full_metadata %>% filter(!(is.na(cdr_Full_ig_hk) & is.na(cdr_Full_ig_hL)))


full_metadata <- full_metadata %>%
  mutate(
    CloneSize_hk = ifelse(
      is.na(cdr_Full_ig_hk),
      NA,
      ave(seq_along(cdr_Full_ig_hk), cdr_Full_ig_hk, FUN = length)
    ),
    CloneSize_hL = ifelse(
      is.na(cdr_Full_ig_hL),
      NA,
      ave(seq_along(cdr_Full_ig_hL), cdr_Full_ig_hL, FUN = length)
    )
  ) %>%
  relocate(CloneSize_hk, .after = cdr_Full_ig_hk) %>%
  relocate(CloneSize_hL, .after = cdr_Full_ig_hL)



full_metadata_HL <- full_metadata %>% filter(!(is.na(full_metadata$CloneSize_hL)))
full_metadata_HK <- full_metadata %>% filter(!(is.na(full_metadata$CloneSize_hk)))


full_metadata = full_metadata_HL

# Filter by diagnosis if needed
metadata_filtered <- full_metadata %>%
  filter(!is.na(CloneSize_hL)) %>%
  mutate(clone_category = case_when(
    CloneSize_hL == 1 ~ "Singleton",
    CloneSize_hL >= 2 & CloneSize_hL <= 10 ~ "Size 2-10",
    CloneSize_hL >= 11 & CloneSize_hL <= 50 ~ "Size 11-50",
    CloneSize_hL >= 51 & CloneSize_hL <= 100 ~ "Size 51-100",
    CloneSize_hL >= 101 ~ "Size 100+",
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
    plot.title = element_text(size = 4, face = "bold", hjust = 0.5),
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
    title = "hL Clonal Size Distribution by Diagnosis (Percentage) | B and Plasma Cells Based on cdr_Full_ig_hL"
  ) +
  white_theme +
  scale_x_discrete(expand = c(0,10)) +
  coord_cartesian(clip = "off")

ggsave("B and Plasma Cells Based on cdr_Full_ig_hL (Percentage).pdf", p1, width = 4, height = 2, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
ggsave("B and Plasma Cells Based on cdr_Full_ig_hL (Percentage).png", p1, width = 4, height = 2, dpi = 600, bg = "white")




p2 <- ggplot(df, aes(x = Diagnosis, y = n_cells, fill = clone_category)) +
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Diagnosis", 
    y = "Number of Cells", 
    fill = "Clone Size",
    title = "hL Clonal Size Distribution by Diagnosis (Raw Counts) | B and Plasma Cells Based on cdr_Full_ig_hL"
  ) +
  white_theme +
  scale_x_discrete(expand = c(0,10)) +
  coord_cartesian(clip = "off") 

ggsave("B and Plasma Cells Based on cdr_Full_ig_hL (Raw Counts).pdf", p2, width = 4, height = 2, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
ggsave("B and Plasma Cells Based on cdr_Full_ig_hL (Raw Counts).png", p2, width = 4, height = 2, dpi = 600, bg = "white")


















full_metadata = full_metadata_HK

# Filter by diagnosis if needed
metadata_filtered <- full_metadata %>%
  filter(!is.na(CloneSize_hk)) %>%
  mutate(clone_category = case_when(
    CloneSize_hk == 1 ~ "Singleton",
    CloneSize_hk >= 2 & CloneSize_hk <= 10 ~ "Size 2-10",
    CloneSize_hk >= 11 & CloneSize_hk <= 50 ~ "Size 11-50",
    CloneSize_hk >= 51 & CloneSize_hk <= 100 ~ "Size 51-100",
    CloneSize_hk >= 101 ~ "Size 100+",
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
    plot.title = element_text(size = 4, face = "bold", hjust = 0.5),
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
    title = "hk Clonal Size Distribution by Diagnosis (Percentage) | B and Plasma Cells Based on cdr_Full_ig_hk"
  ) +
  white_theme +
  scale_x_discrete(expand = c(0,10)) +
  coord_cartesian(clip = "off")

ggsave("B and Plasma Cells Based on cdr_Full_ig_hk (Percentage).pdf", p1, width = 4, height = 2, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
ggsave("B and Plasma Cells Based on cdr_Full_ig_hk (Percentage).png", p1, width = 4, height = 2, dpi = 600, bg = "white")




p2 <- ggplot(df, aes(x = Diagnosis, y = n_cells, fill = clone_category)) +
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Diagnosis", 
    y = "Number of Cells", 
    fill = "Clone Size",
    title = "hk Clonal Size Distribution by Diagnosis (Raw Counts) | B and Plasma Cells Based on cdr_Full_ig_hk"
  ) +
  white_theme +
  scale_x_discrete(expand = c(0,10)) +
  coord_cartesian(clip = "off") 

ggsave("B and Plasma Cells Based on cdr_Full_ig_hk (Raw Counts).pdf", p2, width = 4, height = 2, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
ggsave("B and Plasma Cells Based on cdr_Full_ig_hk (Raw Counts).png", p2, width = 4, height = 2, dpi = 600, bg = "white")
