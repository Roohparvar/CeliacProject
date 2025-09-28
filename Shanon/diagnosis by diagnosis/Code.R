



library(dplyr)
library(ggplot2)
library(vegan)

# انتخاب کلاسترهای هدف برای حذف (اختیاری)
# انتخاب کلاسترهای هدف (اگر میخوای حذفشون کنی)
target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "Tgd INSIG1+", "Tgd", "Tgd CD8+", "ILC2/ILC3", "ILC2/ILTi", "ILC1", "DC", "Macrophages", "Mast cells"
)

full_metadata <- full_metadata %>%
  filter(!cluster %in% target_clusters)

# ترتیب مرجع برای نمودار
ordered_celltypes_bycelltype <- c(
  "CD4-CD8-", "Cyt. IEL", "Trm IEL", "IEL CCL4+", "nIEL",
  "Prolif. IEL", "IEL GZMK+", "CD4-CD8- IL10 ICOS", "CD8 Mem", "CD8 Cyt.",
  "CD8 Trm", "NK Tgd", "Tgd CD8+", "Tgd", "Tgd INSIG1+", "ILC1",
  "ILC2/ILC3", "ILC2/ILTi", "Th", "Tregs", "Th1 Mem", "Th2/Tfh", "Th17",
  "B cells MZB1+", "B cells BAFFR", "B cells_1", "B cells_2",
  "Plasmablast", "Plasma cells_1", "Plasma cells_2",
  "Macrophages", "DC", "Mast cells"
)

ordered_clusters <- ordered_celltypes_bycelltype[
  ordered_celltypes_bycelltype %in% unique(full_metadata$cluster)
]

# محاسبه شاخص‌ها بر اساس Diagnosis و Cluster
cluster_Diagnosis_diversity <- full_metadata %>%
  filter(!is.na(cdr_Full_ab)) %>%
  group_by(Diagnosis, cluster, cdr_Full_ab) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Diagnosis, cluster) %>%
  summarise(
    shannon  = diversity(count, index = "shannon"),
    simpson  = diversity(count, index = "simpson"),
    richness = specnumber(count),
    .groups = "drop"
  ) %>%
  mutate(
    shannon_norm = shannon / log(richness),
    cluster = factor(cluster, levels = ordered_clusters)
  )

# تم گرافیکی
custom_theme <- theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
  )

# حلقه برای رسم و ذخیره نمودار هر بیمار
Diagnosiss <- unique(cluster_Diagnosis_diversity$Diagnosis)

for (p in Diagnosiss) {
  df_Diagnosis <- cluster_Diagnosis_diversity %>% filter(Diagnosis == p)
  
  # نمودار شانون خام
  p1 <- ggplot(df_Diagnosis, aes(x = cluster, y = shannon)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Shannon Diversity - Diagnosis", p),
         x = "Cluster", y = "Shannon Index") +
    custom_theme
  
  # نمودار شانون نرمالایز شده
  p2 <- ggplot(df_Diagnosis, aes(x = cluster, y = shannon_norm)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    labs(title = paste("Normalized Shannon Diversity - Diagnosis", p),
         x = "Cluster", y = "Normalized Shannon Index") +
    custom_theme
  
  # ذخیره نمودارها
  ggsave(paste0("shannon_diversity_Diagnosis_", p, ".png"), plot = p1, width = 10, height = 5, dpi = 300, bg = "white")
  ggsave(paste0("shannon_norm_diversity_Diagnosis_", p, ".png"), plot = p2, width = 10, height = 5, dpi = 300, bg = "white")
}
