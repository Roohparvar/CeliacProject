library(tidyverse)
library(readxl)
library(writexl)

# فیلتر کردن برای فقط hkl
hkl_cells <- full_metadata %>%
  filter(imm_receptor_Esmaeil == "hkl")

# حذف کردن کلاسترهای غیرمطلوب
target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR"
)

hkl_cells <- hkl_cells %>%
  filter(cluster %in% target_clusters)

# اضافه کردن ستون برای نوع ایمنی
hkl_cells <- hkl_cells %>%
  mutate(
    IG_Type = case_when(
      !is.na(cdr_Full_ig_hk) & is.na(cdr_Full_ig_hL) ~ "Only HK",
      is.na(cdr_Full_ig_hk) & !is.na(cdr_Full_ig_hL) ~ "Only HL",
      !is.na(cdr_Full_ig_hk) & !is.na(cdr_Full_ig_hL) ~ "Both",
      TRUE ~ "None"
    )
  ) %>%
  filter(IG_Type != "None")  # حذف مواردی که هیچکدام را ندارند

# شمارش تعداد هر نوع در هر کلاستر
plot_df <- hkl_cells %>%
  group_by(cluster, IG_Type) %>%
  summarise(Count = n(), .groups = "drop")

# رسم نمودار
p <- ggplot(plot_df, aes(x = cluster, y = Count, fill = IG_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of IG Types in Clusters",
       x = "Cluster", y = "Number of Cells") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Only HK" = "#C0C0C0", "Only HL" = "#333333", "Both" = "#87CEFA"))

# ذخیره نمودار به صورت PNG با بک‌گراند سفید
ggsave("IG_type_distribution.png", plot = p, width = 10, height = 6, dpi = 300, bg = "white")
