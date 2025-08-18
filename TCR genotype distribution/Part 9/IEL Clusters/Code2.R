library(tibble)
library(dplyr)
library(ggplot2)
library(ggalluvial)
library(tidyr)
library(scales)


target_clusters <- c("IEL GZMK+", "Trm IEL", "Prolif. IEL", "Cyt. IEL", "IEL CCL4+", "nIEL")
target_diagnosis <- c("Healthy", "ACD", "RCD-I", "RCD-II")

percent_matrix <- matrix(NA, 
                         nrow = length(target_diagnosis), 
                         ncol = length(target_clusters),
                         dimnames = list(target_diagnosis, target_clusters))

for (diag in target_diagnosis) {
  subset_diag <- full_metadata[full_metadata$Diagnosis == diag, ]
  
  for (clust in target_clusters) {
    count_cluster <- sum(subset_diag$cluster == clust, na.rm = TRUE)
    total_cells <- nrow(subset_diag)
    percent <- (count_cluster / total_cells) * 100
    percent_matrix[diag, clust] <- percent
  }
}

percent_matrix <- t(percent_matrix)

df <- as.data.frame(percent_matrix)
df <- cbind(cluster = rownames(df), df)
rownames(df) <- NULL



wide_percent <- as_tibble(df)




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


cluster_colors <- c(
  "IEL GZMK+" = "#4A4E69",
  "Trm IEL"    = "#2a9d8f",  
  "Prolif. IEL" = "#e76f51",  
  "Cyt. IEL"   = "#8d99ae",
  "IEL CCL4+" = "#8d9900",
  "nIEL" = "#006f00"
)



# رسم نمودار
p <- ggplot(long_data,
            aes(x = Stage, stratum = id, alluvium = alluvium,
                y = Freq, fill = id)) +
  geom_flow(alpha = 0.7, color = "grey50") +
  geom_stratum(width = 0.3, color = "black") +
  geom_text(stat = "stratum",
            aes(label = ifelse(Freq >= 0.4, paste0(round(Freq, 1), "%"), "")),
            size = 2, color = "black") +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_fill_manual(values = cluster_colors) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "IEL T Cell Clusters Distribution (Percentage)",
       x = "Diagnosis",
       y = "Percentage (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



# ذخیره نمودار به PNG
ggsave("IEL_alluvial_percentages_Healthy_ACD_RCDI_RCDII.png",
       plot = p, width = 10, height = 5, dpi = 300, bg = "white")




