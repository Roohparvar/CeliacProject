library(dplyr)
library(ggplot2)

# فیلتر فقط ردیف‌هایی که TRBV28 دارند
trbv28_data <- full_metadata %>%
  filter(TRBV == "TRBV28") %>%
  filter(Diagnosis %in% c("Healthy", "ACD", "RCD-I", "RCD-II"))

# شمارش تعداد سلول‌ها با TRBV28 در هر بیمار
gene_counts <- trbv28_data %>%
  group_by(Diagnosis, Patient) %>%
  summarise(count = n(), .groups = "drop")

# نرمال‌سازی نسبت به کل سلول‌های هر بیمار
total_cells <- full_metadata %>%
  group_by(Diagnosis, Patient) %>%
  summarise(total = n(), .groups = "drop")

# ترکیب و محاسبه expression نرمال‌شده
normalized_data <- left_join(gene_counts, total_cells, by = c("Diagnosis", "Patient")) %>%
  mutate(norm_expr = count / total)

# رسم نمودار
p <- ggplot(normalized_data, aes(x = Diagnosis, y = norm_expr, fill = Diagnosis)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.6) +
  labs(
    title = "Normalized Expression of TRBV28 across Diagnoses",
    x = "Diagnosis Group",
    y = "Normalized Expression (TRBV28)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

# چاپ نمودار در R
print(p)

# ذخیره به صورت تصویر
ggsave("TRBV28_expression_boxplot.png", plot = p, width = 8, height = 6, dpi = 300, bg = "white")
