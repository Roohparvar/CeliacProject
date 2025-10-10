setwd("C:/Esmaeil/CeliacProject/CeliacProject/4_Clonal Expansion Analysis/γδ/shannon entropy")



library(dplyr)
library(ggplot2)
library(vegan)


full_metadata$cluster <- recode(full_metadata$cluster,
                                "Tgd INSIG1+" = "Tγδ INSIG1+",
                                "NK/Tgd" = "NK/Tγδ",
                                "Act. Tgd" = "Act. Tγδ",
                                "Tgd CD8+" = "Tγδ CD8+"
)



target_clusters <- c(
  "Trm IEL", "NK/Tγδ", "Tγδ CD8+", "Act. Tγδ", "Tγδ INSIG1+"
)

full_metadata <- full_metadata %>%
  filter(cluster %in% target_clusters)

# ترتیب مرجع برای نمودار
ordered_celltypes_bycelltype <- c(
  "Trm IEL", "NK/Tγδ", "Tγδ CD8+", "Act. Tγδ", "Tγδ INSIG1+"
)

ordered_clusters <- ordered_celltypes_bycelltype[
  ordered_celltypes_bycelltype %in% unique(full_metadata$cluster)
]

# محاسبه شاخص‌ها بر اساس Diagnosis و Cluster
cluster_Diagnosis_diversity <- full_metadata %>%
  filter(!is.na(cdr_Full_gd)) %>%
  group_by(Diagnosis, cluster, cdr_Full_gd) %>%
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
    plot.title = element_text(hjust = 0.5, , size = 10),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
  )

# حلقه برای رسم و ذخیره نمودار هر بیمار
Diagnosiss <- unique(cluster_Diagnosis_diversity$Diagnosis)




# Define color mapping for each diagnosis
diagnosis_colors <- c(
  "Healthy" = "#D83A8A",
  "ACD"     = "#349C7C",
  "RCD-I"   = "#D0632B",
  "RCD-II"  = "#6E71AD"
)

for (p in Diagnosiss) {
  df_Diagnosis <- cluster_Diagnosis_diversity %>% filter(Diagnosis == p)
  
  # انتخاب رنگ بر اساس تشخیص
  fill_color <- diagnosis_colors[p]
  
  
  # نمودار شانون خام
  p1 <- ggplot(df_Diagnosis, aes(x = cluster, y = shannon)) +
    geom_bar(stat = "identity", fill = fill_color, width = 0.9) +
    labs(title = paste("Shannon Diversity - ", p),
         x = "Cluster", y = "Shannon Index") +
    custom_theme
  
  # نمودار شانون نرمالایز شده
  p2 <- ggplot(df_Diagnosis, aes(x = cluster, y = shannon_norm)) +
    geom_bar(stat = "identity", fill = fill_color, width = 0.9) +
    labs(title = paste("Normalized Shannon Diversity - ", p),
         x = "Cluster", y = "Normalized Shannon Index") +
    custom_theme
  
  # ذخیره نمودارها (PNG)
  ggsave(paste0("shannon_diversity_Diagnosis_", p, ".png"), plot = p1,
         width = 3, height = 4, dpi = 600, bg = "white")
  ggsave(paste0("shannon_norm_diversity_Diagnosis_", p, ".png"), plot = p2,
         width = 3, height = 4, dpi = 600, bg = "white")
  
  # ذخیره نمودارها (PDF)
  ggsave(paste0("shannon_diversity_Diagnosis_", p, ".pdf"), plot = p1,
         width = 3, height = 4, dpi = 600, bg = "white")
  ggsave(paste0("shannon_norm_diversity_Diagnosis_", p, ".pdf"), plot = p2,
         width = 3, height = 4, dpi = 600, bg = "white")
}


