library(dplyr)
library(tidyr)
library(ggplot2)
library(scRepertoire)


full_metadata = full_metadata[full_metadata$cluster == "CD4-CD8-" | full_metadata$cluster == "Cyt. IEL", ]


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



# Define the desired patient order
patient_order_Lymph <- c(
  "H-3", "H-8", "H-9", "H-10", "H-11", "H-12",
  "ACD-1", "ACD-2", "ACD-5", "ACD-6", "ACD-7", "ACD-8", "ACD-9",
  "RCD1-1a", "RCD1-2", "RCD1-3", "RCD1-6a", "RCD1-11", "RCD1-12",
  "RCD1-13", "RCD1-14", "RCD1-15", "RCD2-1b", "RCD2-2"
)

# Convert Patient to a factor with specified order
df$Patient <- factor(df$Patient, levels = patient_order_Lymph)


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
    title = "αβ Clonal Size Distribution by Patient (Percentage) | CD4-CD8- and Cyt. IEL"
  ) +
  white_theme +
  scale_x_discrete(expand = c(0,10)) +   # حذف فاصله اضافی دسته‌ها
  coord_cartesian(clip = "off")          # اجازه می‌دهد نمودار کامل نشان داده شود بدون فضای اضافه

ggsave("CD4-CD8- and Cyt IEL (Percentage).pdf", p1, width = 4, height = 2, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
ggsave("CD4-CD8- and Cyt IEL (Percentage).png", p1, width = 4, height = 2, dpi = 600, bg = "white")




p2 <- ggplot(df, aes(x = Patient, y = n_cells, fill = clone_category)) +
  geom_bar(stat = "identity", width = 0.9) +
  scale_fill_manual(values = clone_colors) +
  labs(
    x = "Patient", 
    y = "Number of Cells", 
    fill = "Clone Size",
    title = "αβ Clonal Size Distribution by Patient (Raw Counts) | CD4-CD8- and Cyt. IEL"
  ) +
  white_theme +
  scale_x_discrete(expand = c(0,10)) +   # حذف فاصله اضافی دسته‌ها
  coord_cartesian(clip = "off")          # اجازه می‌دهد نمودار کامل نشان داده شود بدون فضای اضافه

ggsave("CD4-CD8- and Cyt IEL (Raw Counts).pdf", p2, width = 4, height = 2, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
ggsave("CD4-CD8- and Cyt IELs (Raw Counts).png", p2, width = 4, height = 2, dpi = 600, bg = "white")
