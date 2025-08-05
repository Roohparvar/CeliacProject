library(dplyr)
library(ggplot2)
library(ggalluvial)
library(tidyr)
library(scales)  # برای percent_format

# تعریف کلاسترهای مورد نظر
cluster_order <- c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs")

# فیلتر داده‌ها برای 4 Diagnosis مورد نظر
filtered_data <- full_metadata %>%
  filter(Diagnosis %in% c("Healthy", "ACD", "RCD-I", "RCD-II"), cluster %in% cluster_order)

# شمارش تعداد سلول‌ها در هر cluster و Diagnosis
count_data <- filtered_data %>%
  group_by(Diagnosis, cluster) %>%
  summarise(n = n(), .groups = "drop")

# محاسبه درصد در هر Diagnosis
percent_data <- count_data %>%
  group_by(Diagnosis) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup()

# تبدیل داده‌ها به فرمت wide (هر Diagnosis یک ستون درصد)
wide_percent <- percent_data %>%
  select(Diagnosis, cluster, percentage) %>%
  pivot_wider(names_from = Diagnosis, values_from = percentage, values_fill = 0)

# نامگذاری ستون‌ها
alluvial_data <- wide_percent %>%
  rename(Healthy = Healthy, ACD = ACD, RCD_I = `RCD-I`, RCD_II = `RCD-II`) %>%
  mutate(id = cluster)

# ساخت داده long برای 4 مرحله (با درصد)
long_data <- alluvial_data %>%
  pivot_longer(cols = c("Healthy", "ACD", "RCD_I", "RCD_II"),
               names_to = "Stage",
               values_to = "Freq")

# ترتیب factor محور x
long_data$Stage <- factor(long_data$Stage, levels = c("Healthy", "ACD", "RCD_I", "RCD_II"))

# اضافه کردن شناسه alluvium
long_data <- long_data %>%
  arrange(id, Stage) %>%
  group_by(id) %>%
  mutate(alluvium = cur_group_id()) %>%
  ungroup()

# تعریف رنگ‌های تیره و ملایم برای 4 کلاستر
cluster_colors <- c(
  "Th1 Mem" = "#4A4E69",   # بنفش خاکستری تیره (Dark Slate Purple)
  "Th17"    = "#2a9d8f",   # سبز متمایل به آبی
  "Th2/Tfh" = "#e76f51",   # آجری مایل به قهوه‌ای
  "Tregs"   = "#8d99ae"    # خاکستری آبی تیره
)

# رسم نمودار
p <- ggplot(long_data,
            aes(x = Stage, stratum = id, alluvium = alluvium,
                y = Freq, fill = id)) +
  geom_flow(alpha = 0.7, color = "grey50") +
  geom_stratum(width = 0.3, color = "black") +
  geom_text(stat = "stratum", aes(label = paste0(round(Freq, 1), "%")),
            size = 3, color = "black") +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_fill_manual(values = cluster_colors) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "CD4+ T Cell Cluster Distribution (Percentage)",
       x = "Diagnosis",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

print(p)

# ذخیره نمودار به PNG
ggsave("CD4_alluvial_percentages_Healthy_ACD_RCDI_RCDII.png",
       plot = p, width = 10, height = 5, dpi = 300, bg = "white")
