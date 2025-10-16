library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(rlang)

# Step 1: فقط کلاستر CD4-CD8-
cd4cd8_data <- full_metadata %>%
  filter(cluster == "Cyt. IEL") %>%
  mutate(group = case_when(
    imm_receptor_Esmaeil %in% c("Aberrant ab", "Aberrant g") ~ "Aberrant",
    TRUE ~ "Normal"
  ))

# Step 2: گرفتن ژن‌های TRAV و TRBV برای این داده
trav_data <- cd4cd8_data %>%
  filter(!is.na(TRAV) & TRAV != "") %>%
  select(group, gene = TRAV)

trbv_data <- cd4cd8_data %>%
  filter(!is.na(TRBV) & TRBV != "") %>%
  select(group, gene = TRBV)

# Step 3: ادغام و برچسب‌گذاری
combined_data <- bind_rows(
  trav_data %>% mutate(chain = "TRAV"),
  trbv_data %>% mutate(chain = "TRBV")
)

# Step 4: شمارش و نرمال‌سازی
dot_data <- combined_data %>%
  group_by(group, chain, gene) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(group, chain) %>%
  mutate(freq = count / sum(count)) %>%
  ungroup() %>%
  group_by(chain) %>%
  mutate(gene = fct_reorder(gene, freq, .desc = TRUE))

# Step 5: تابع برای رسم dotplot
make_dotplot <- function(data, title, x_var = "gene", y_var = "group", x_font_size = 10, y_font_size = 10) {
  ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_point(aes(size = freq, color = chain), alpha = 0.7) +
    scale_size_continuous(name = "Frequency") +
    scale_color_manual(values = c("TRAV" = "#1f77b4", "TRBV" = "#ff7f0e")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = x_font_size),
      axis.text.y = element_text(size = y_font_size),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(x = x_var, y = y_var, title = title)
}

# Step 6: رسم نمودارها (با سطر = Aberrant / Normal و ستون = Gene)

# ترکیب‌شده (TRAV + TRBV)
dot_plot_all <- make_dotplot(dot_data, "TCR V Gene Usage in Cyt. IEL Clusters", x_font_size = 8, y_font_size = 12)

# فقط TRAV
dot_plot_trav <- make_dotplot(
  filter(dot_data, chain == "TRAV"),
  "TRAV Usage in Cyt. IEL Clusters",
  x_font_size = 8,
  y_font_size = 12
)

# فقط TRBV
dot_plot_trbv <- make_dotplot(
  filter(dot_data, chain == "TRBV"),
  "TRBV Usage in CD4-CD8- Clusters",
  x_font_size = 8,
  y_font_size = 12
)

# Step 7: ذخیره‌سازی نمودارها
ggsave("1_Cyt. IEL_TCR_dotplot_all.png", plot = dot_plot_all, width = 20, height = 5, dpi = 300, bg = "white")
ggsave("2_Cyt. IEL_TCR_dotplot_TRAV.png", plot = dot_plot_trav, width = 20, height = 5, dpi = 300, bg = "white")
ggsave("3_Cyt. IEL_TCR_dotplot_TRBV.png", plot = dot_plot_trbv, width = 20, height = 5, dpi = 300, bg = "white")













