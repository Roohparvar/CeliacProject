setwd("C:/Esmaeil/CeliacProject/CeliacProject/2_immune receptors distribution/immune receptors distribution After Data Cleaning/Bubble plot")

library(dplyr)
library(ggplot2)


full_metadata$imm_receptor_Esmaeil <- recode(full_metadata$imm_receptor_Esmaeil,
                                             "Aberrant ab" = "Aberrant αβ",
                                             "Aberrant g" = "Aberrant γ",
                                             "ab" = "αβ",
                                             "gd" = "γδ"
)
full_metadata$cluster <- recode(full_metadata$cluster,
                                "Tgd INSIG1+" = "Tγδ INSIG1+",
                                "NK/Tgd" = "NK/Tγδ",
                                "Act. Tgd" = "Act. Tγδ",
                                "Tgd CD8+" = "Tγδ CD8+"
)


# Summarize counts per cluster and receptor, remove NAs
bubble_data <- full_metadata %>%
  filter(!is.na(imm_receptor_Esmaeil)) %>%
  group_by(cluster, imm_receptor_Esmaeil) %>%
  summarise(count = n(), .groups = "drop")

# ----------------------------
# ترتیب دلخواه کلاسترها (محور x)
cluster_order <- c(
  "CD4-CD8-", "T eff. IEL", "Trm IEL", 
  
  "Act. plasma IGHA+", "Mature plasma IGHA+", "Plasma IGHG+", 
  "Mem B cells", "Homing plasmablast", "Act. plasmablast", "B cells BAFFR+",
  
  "CD4 Trm", 
  
  "Th17", "Cyt. IEL", "IEL GZMK+", "CD8 Trm", "Tfh", "Tγδ CD8+", "Tregs", "CD8 Mem", "CD4 FTH1+", "Prolif. IEL", "IEL CCL4+", "ILC1/ILC2",
  "NK/Tγδ", "nIEL", "Tγδ INSIG1+", "CD4-CD8-IL10+", "Act. Tγδ", "ILC2/ILTi",  "ILC3",
  "Macrophages", "pDC", "Mast cells"
)
bubble_data$cluster <- factor(bubble_data$cluster, levels = cluster_order)

# ----------------------------
# ترتیب دلخواه ایمنی‌رسانه‌ها (محور y)
receptor_order <- c("αβ", "γδ", "hkl", "Aberrant γ", "Aberrant αβ")
bubble_data$imm_receptor_Esmaeil <- factor(bubble_data$imm_receptor_Esmaeil,
                                           levels = receptor_order)

# ----------------------------
# تعریف رنگ‌ها
custom_colors <- c(
  "αβ" = "#ee1819",
  "γδ" = "#fd7d00",
  "hkl" = "#fcd919",
  "Aberrant αβ" = "#3a78ce",
  "Aberrant γ" = "#47ad45"
)

legend_order <- c( "Aberrant αβ", "Aberrant γ", "hkl", "γδ", "αβ")

# ----------------------------
# ایجاد نمودار bubble/horizontal dot
bubble_plot <- ggplot(bubble_data, aes(x = cluster, y = imm_receptor_Esmaeil)) +
  geom_point(aes(size = count, color = imm_receptor_Esmaeil)) +
  scale_color_manual(values = custom_colors, breaks = legend_order) +
  scale_size(range = c(0, 9)) +
  scale_x_discrete(expand = expansion(mult = c(0.01,0.01))) + 
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    title = "Receptor Distribution Across Clusters",
    x = "Cluster",
    y = "Receptor Type",
    size = "Cell Count",
    color = "Receptor Type"
  )

# ----------------------------
# ذخیره تصویر
ggsave("Receptor_Bubble_Plot_Horizontal_Adjusted.png", plot = bubble_plot, width = 11.5, height = 4, dpi = 600, bg = "white")
ggsave("Receptor_Bubble_Plot_Horizontal_Adjusted.pdf", plot = bubble_plot, width = 11.5, height = 4, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")