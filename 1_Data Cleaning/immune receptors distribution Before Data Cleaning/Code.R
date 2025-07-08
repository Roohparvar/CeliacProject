#--------------------------------------------------------------------------------- Add UMAP plot colored by imm_receptor with custom colors
library(ggplot2)
library(dplyr)

receptor_colors <- c(
  "ab" = "#ee1819",
  "gd" = "#fd7d00",
  "abgd" = "#92509f",
  "T and B" = "#9b592d",
  "hkl" = "#fcd919",
  "Aberrant ab" = "#3a78ce",
  "Aberrant g" = "#47ad45",
  "None" = "#eeeeee"
)

full_metadata$imm_receptor <- factor(
  full_metadata$imm_receptor,
  levels = c("", "ab", "Aberrant ab", "Aberrant g", "gd", "hkl", "abgd", "T and B")
)

receptor_labels <- c(
  "ab" = "TCR⍺β",
  "gd" = "γδ",
  "abgd" = "⍺βγδ",
  "T and B" = "T and B",
  "hkl" = "hkl",
  "Aberrant ab" = "Aberrant ⍺β",
  "Aberrant g" = "Aberrant γ"
)

p <- ggplot() +
  geom_point(
    data = full_metadata %>% filter(imm_receptor == ""),
    aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = imm_receptor),
    size = 0.2, alpha = 0.5
  ) +
  geom_point(
    data = full_metadata %>% filter(imm_receptor != ""),
    aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = imm_receptor),
    size = 0.2, alpha = 0.8
  ) +
  scale_color_manual(values = receptor_colors, labels = receptor_labels) +
  guides(color = guide_legend(override.aes = list(size = 4))) +  
  labs(
    title = "UMAP Colored by immune receptor",
    x = "UMAP 1",
    y = "UMAP 2",
    color = "Receptor Type"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Umap_imm_receptor_highlighted.png", plot = p, width = 8, height = 6, dpi = 300, bg = "white")


