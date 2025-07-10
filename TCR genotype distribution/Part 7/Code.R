library(dplyr)
library(ggplot2)

# ترتیب دلخواه برای TRBJ ها
ordered_trbj <- c(
  "TRBJ1-1", "TRBJ1-2", "TRBJ1-3", "TRBJ1-4", "TRBJ1-5", "TRBJ1-6",
  "TRBJ2-1", "TRBJ2-2", "TRBJ2-3", "TRBJ2-4", "TRBJ2-5", "TRBJ2-6", "TRBJ2-7"
)

# فیلتر فقط سلول‌هایی که TRBV28 هستند و TRBJ و CDR3 دارند
trbv28_data <- full_metadata %>%
  filter(TRBV == "TRBV28", !is.na(TRBJ), !is.na(cdr_Full_ab))

# محاسبه درصد استفاده از TRBJها
trbj_usage <- trbv28_data %>%
  group_by(TRBJ) %>%
  summarise(unique_cdr3_count = n_distinct(cdr_Full_ab)) %>%
  mutate(percentage = 100 * unique_cdr3_count / sum(unique_cdr3_count)) %>%
  filter(TRBJ %in% ordered_trbj) %>%
  mutate(TRBJ = factor(TRBJ, levels = ordered_trbj))  # مرتب‌سازی دستی محور x

# رسم نمودار
p <- ggplot(trbj_usage, aes(x = TRBJ, y = percentage)) +
  geom_col(fill = "black", width = 0.6) +  # عرض میله‌ها کاهش یافت
  labs(
    title = "TRBJ Gene Usage in TRBV28 Clonotypes",
    x = "TRBJ Gene",
    y = "Percentage of Unique CDR3s"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


# نمایش نمودار
print(p)

# ذخیره تصویر
ggsave("TRBJ_usage_TRBV28_ordered.png", plot = p, width = 8, height = 6, dpi = 300, bg = "white")
