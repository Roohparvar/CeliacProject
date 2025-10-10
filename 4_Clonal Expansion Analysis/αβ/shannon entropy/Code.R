setwd("C:/Esmaeil/CeliacProject/CeliacProject/4_Clonal Expansion Analysis/αβ/shannon entropy")


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
  "Act. plasma IGHA+", "Act. plasmablast", "B cells BAFFR+", "Homing plasmablast",
  "Macrophages", "Mast cells", "Mature plasma IGHA+", "Mem B cells", "Plasma IGHG+", "pDC"
)


full_metadata <- full_metadata %>%
  filter(!cluster %in% target_clusters)


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
    cluster = factor(cluster)
  )


custom_theme <- theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1)
  )


Diagnosiss <- unique(cluster_Diagnosis_diversity$Diagnosis)


diagnosis_colors <- c(
  "Healthy" = "#D83A8A",
  "ACD"     = "#349C7C",
  "RCD-I"   = "#D0632B",
  "RCD-II"  = "#6E71AD"
)


cluster_order <- c(
  "CD4-CD8-", "T eff. IEL", "Trm IEL", "IEL CCL4+", "nIEL", "Prolif. IEL", "IEL GZMK+", "Cyt. IEL", "CD4-CD8-IL10+",
  "CD8 Mem", "CD8 Trm", "CD4 FTH1+", "Tregs", "CD4 Trm", "Tfh", "Th17", "NK/Tγδ", "Tγδ CD8+", "Act. Tγδ", "Tγδ INSIG1+",
  "ILC1/ILC2", "ILC2/ILTi", "ILC3"
)


# Convert cluster to factor with the specified order
cluster_Diagnosis_diversity <- cluster_Diagnosis_diversity %>%
  mutate(cluster = factor(cluster, levels = cluster_order))


# Now the plotting loop will respect this order
for (p in Diagnosiss) {
  df_Diagnosis <- cluster_Diagnosis_diversity %>% filter(Diagnosis == p)
  
  fill_color <- diagnosis_colors[p]
  
  p1 <- ggplot(df_Diagnosis, aes(x = cluster, y = shannon)) +
    geom_bar(stat = "identity", fill = fill_color) +
    labs(title = paste("Shannon Diversity - Diagnosis", p),
         x = "Cluster", y = "Shannon Index") +
    custom_theme
  
  p2 <- ggplot(df_Diagnosis, aes(x = cluster, y = shannon_norm)) +
    geom_bar(stat = "identity", fill = fill_color) +
    labs(title = paste("Normalized Shannon Diversity - Diagnosis", p),
         x = "Cluster", y = "Normalized Shannon Index") +
    custom_theme
  
  ggsave(paste0("shannon_diversity_Diagnosis_", p, ".png"), plot = p1, width = 10, height = 5, dpi = 300, bg = "white")
  ggsave(paste0("shannon_norm_diversity_Diagnosis_", p, ".png"), plot = p2, width = 10, height = 5, dpi = 300, bg = "white")
  ggsave(paste0("shannon_diversity_Diagnosis_", p, ".pdf"), plot = p1, width = 10, height = 5, dpi = 300, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
  ggsave(paste0("shannon_norm_diversity_Diagnosis_", p, ".pdf"), plot = p2, width = 10, height = 5, dpi = 300, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
}
