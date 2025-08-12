library(dplyr)
library(tidyr)

cluster_order <- c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs")
cluster_Diagnosis <- c("ACD")
filtered_metadata <- full_metadata %>%
  filter(
    !is.na(TRAV) & TRAV != "",
    !is.na(TRBV) & TRBV != "",
    cluster %in% cluster_order,
    Diagnosis %in% cluster_Diagnosis,
  )

filtered_metadata <- filtered_metadata %>% distinct(cdr_Full_ab, .keep_all = TRUE)

cluster_counts <- filtered_metadata %>%
  group_by(cluster) %>%
  summarise(count = n()) %>%
  complete(cluster = cluster_order, fill = list(count = 0))  
total_cells <- nrow(filtered_metadata)
cluster_counts <- cluster_counts %>%
  add_row(cluster = "All", count = total_cells)






trav_cluster_counts <- filtered_metadata %>%
  filter(TRAV != "") %>%
  group_by(TRAV, cluster) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(TRAV, cluster = cluster_order, fill = list(count = 0))

trav_wide <- trav_cluster_counts %>% pivot_wider(names_from = cluster, values_from = count)
trav_wide <- trav_wide %>% mutate(All = rowSums(across(all_of(cluster_order))))
trav_wide <- trav_wide %>% select(TRAV, all_of(cluster_order), All)






trbv_cluster_counts <- filtered_metadata %>%
  filter(TRBV != "") %>%
  group_by(TRBV, cluster) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(TRBV, cluster = cluster_order, fill = list(count = 0))  # جایگزین 0 برای غایب‌ها

# تبدیل داده به حالت Wide (هر کلاستر ستون جدا)
trbv_wide <- trbv_cluster_counts %>%
  pivot_wider(names_from = cluster, values_from = count)

# اضافه کردن ستون تعداد کل تکرار هر TRBV
trbv_wide <- trbv_wide %>%
  mutate(All = rowSums(across(all_of(cluster_order))))

# مرتب کردن ستون‌ها
trbv_wide <- trbv_wide %>%
  select(TRBV, all_of(cluster_order), All)







combo_cluster_counts <- filtered_metadata %>%
  filter(TRAV != "", TRBV != "") %>%
  group_by(TRAV, TRBV, cluster) %>%
  summarise(count = n(), .groups = "drop") %>%
  complete(TRAV, TRBV, cluster = cluster_order, fill = list(count = 0))

# تبدیل به wide format: هر کلاستر یک ستون
combo_wide <- combo_cluster_counts %>%
  pivot_wider(names_from = cluster, values_from = count)

# اضافه کردن ستون تعداد کل تکرار هر ترکیب
combo_wide <- combo_wide %>%
  mutate(All = rowSums(across(all_of(cluster_order))))

# مرتب‌سازی ستون‌ها
combo_wide <- combo_wide %>%
  select(TRAV, TRBV, all_of(cluster_order), All)






# از combo_wide سطرهای ترکیبی که TRAV و TRBV مشخص شده
combo_selected <- combo_wide %>%
  filter(
    (TRAV == "TRAV26-1" & TRBV == "TRBV7-2") |
      (TRAV == "TRAV4" & TRBV == "TRBV4") |
      (TRAV == "TRAV4" & TRBV == "TRBV7-2")
  )



new_row <- data.frame(
  TRAV = "TRAV4",
  TRBV = "TRBV4"
)
for (col in c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs", "All")) {
  new_row[[col]] <- 0
}
combo_selected <- bind_rows(
  combo_selected[1, ],
  new_row,
  combo_selected[-1, ]
)


# از trbv_wide سطرهای TRBV مشخص شده
trbv_selected <- trbv_wide %>%
  filter(TRBV %in% c("TRBV20-1", "TRBV29-1", "TRBV9"))






library(tibble)  # برای استفاده از deframe()


# گرفتن تعداد کل هر کلاستر به صورت named vector
cluster_totals <- cluster_counts %>%
  filter(cluster %in% c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs", "All")) %>%
  select(cluster, count) %>%
  deframe()

# تابع تبدیل مقادیر ماتریس به درصد
convert_to_percent <- function(df) {
  df %>%
    mutate(
      `Th1 Mem` = (`Th1 Mem` / cluster_totals["Th1 Mem"]) * 100,
      Th17 = (Th17 / cluster_totals["Th17"]) * 100,
      `Th2/Tfh` = (`Th2/Tfh` / cluster_totals["Th2/Tfh"]) * 100,
      Tregs = (Tregs / cluster_totals["Tregs"]) * 100,
      All = (All / cluster_totals["All"]) * 100
    )
}

# اعمال تابع به هر دو ماتریس
combo_selected_percent <- convert_to_percent(combo_selected)
trbv_selected_percent <- convert_to_percent(trbv_selected)





library(dplyr)

# اضافه کردن ستون Combination به combo_selected_percent (برای راحتی نامگذاری)
combo_selected_percent <- combo_selected_percent %>%
  mutate(Combination = paste(TRAV, "+", TRBV)) %>%
  select(Combination, everything(), -TRAV, -TRBV)

# اضافه کردن ستون Combination به trbv_selected_percent (برای راحتی نامگذاری)
trbv_selected_percent <- trbv_selected_percent %>%
  mutate(Combination = TRBV) %>%
  select(Combination, everything(), -TRBV)

# ترکیب دو جدول
result <- bind_rows(combo_selected_percent, trbv_selected_percent)

# مرتب کردن ستون‌ها به شکل دلخواه (Combination، 5 ستون درصدی)
result <- result %>%
  select(Combination, `Th1 Mem`, Th17, `Th2/Tfh`, Tregs, All)

# نام ستون‌ها رو به فرمتی که برای رسم لازم داریم تغییر میدیم
colnames(result) <- c("Combination", "Th1_Mem_pct", "Th17_pct", "Th2_Tfh_pct", "Tregs_pct", "All_pct")












library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)


tcr_colors <- c(
  "TRAV26-1 + TRBV7-2" = "#ae1e28",
  "TRAV4 + TRBV4"      = "#f78454",
  "TRAV4 + TRBV7-2"    = "#ffd8c5",
  "TRBV20-1"           = "#cfe3f6",
  "TRBV29-1"           = "#5eaada",
  "TRBV9"              = "#1367b7"
)


library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# فرض بر اینکه result از قبل موجوده
# و tcr_colors تعریف شده

# Long format
long_result <- result %>%
  pivot_longer(
    cols = ends_with("_pct"),
    names_to = "Cluster",
    values_to = "Percent"
  ) %>%
  mutate(Cluster = str_remove(Cluster, "_pct"))

# رسم پلات
p <- ggplot(long_result, aes(x = Cluster, y = Percent, fill = Combination)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Proportion of CD4+ T cells (%)", title = "Based TRAV/TRBV usage") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5) # center the title
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(
    values = tcr_colors,
    na.value = "grey80"  # رنگ خاکستری برای ترکیب‌هایی که تو tcr_colors نیستند
  )



ggsave("Cluster_Stacked_by_Combination.png", plot = p, width = 10, height = 6, dpi = 300, bg = "white")








# فیلتر کردن فقط 3 کلاستر مد نظر
long_result_filtered <- long_result %>%
  filter(Cluster %in% c("Tregs", "Th2_Tfh", "All"))

long_result_filtered <- long_result %>%
  filter(Cluster %in% c("Tregs", "Th2_Tfh", "All")) %>%
  mutate(Cluster = if_else(Cluster == "All", "All CD4", Cluster))

p <- ggplot(long_result_filtered, aes(x = Cluster, y = Percent, fill = Combination)) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Proportion of CD4+ T cells (%)", title = "Based TRAV/TRBV usage") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(hjust = 0.5)  
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(
    values = tcr_colors,
    na.value = "grey80"
  )


ggsave("Cluster_Stacked_by_Combination_Selected.png", plot = p, width = 10, height = 6, dpi = 300, bg = "white")


