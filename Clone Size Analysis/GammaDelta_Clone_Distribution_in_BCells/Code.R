library(ggplot2)
library(dplyr)
library(openxlsx)


target_clusters <- c(
  "Mast cells", "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Macrophages", "Plasmablast",
  "B cells BAFFR", "DC"
)



full_metadata <- full_metadata %>%
  mutate(highlight = ifelse(!is.na(cdr_Full_gd) & cluster %in% target_clusters,
                            "γδ TCR Clones in Target Clusters", NA))


gray_cells <- full_metadata %>% filter(is.na(highlight))
red_cells  <- full_metadata %>% filter(!is.na(highlight))



p <- ggplot() +
  geom_point(data = gray_cells,
             aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2),
             color = "gray80", size = 0.3, alpha = 0.7) +
  geom_point(data = red_cells,
             aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = highlight),
             size = 1, alpha = 0.9) +
  scale_color_manual(values = c("γδ TCR Clones in B-cell Clusters" = "red")) +
  labs(x = "UMAP 1", y = "UMAP 2", color = "") +  # "" برای حذف عنوان رنگ
  theme_classic(base_size = 10) +
  theme(
    legend.position = "right",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )



ggsave("UMAP.png", plot = p, width = 8, height = 6, dpi = 400, bg = "white")



write.xlsx(red_cells, file = "gdT_Clones_in_Bcells.xlsx", rowNames = FALSE)
