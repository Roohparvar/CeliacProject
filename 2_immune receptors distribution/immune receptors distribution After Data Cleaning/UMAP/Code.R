setwd("C:/Esmaeil/CeliacProject/CeliacProject/2_immune receptors distribution/immune receptors distribution After Data Cleaning/UMAP")



#------------------------------------------------------------------------------- libraries
library(ggplot2)
library(dplyr)
library(cowplot)



# Define receptor colors and labels
receptor_colors <- c(
  "ab" = "#ee1819",
  "gd" = "#fd7d00",
  "hkl" = "#fcd919",
  "Aberrant ab" = "#3a78ce",
  "Aberrant g" = "#47ad45",
  "None" = "#eeeeee"
)

receptor_labels <- c(
  "ab" = expression(TCR*alpha*beta),
  "gd" = expression(TRR*gamma*delta),
  "hkl" = "hkl",
  "Aberrant ab" = expression(Aberrant*alpha*beta),
  "Aberrant g" = expression(Aberrant*gamma)
)

# Set factor levels
full_metadata$imm_receptor_Esmaeil <- factor(
  full_metadata$imm_receptor_Esmaeil,
  levels = c("ab", "gd", "Aberrant ab", "Aberrant g", "hkl", "None")
)

# Main UMAP plot (without legend)
main_plot <- ggplot() +
  geom_point(
    data = full_metadata %>% filter(imm_receptor_Esmaeil != ""),
    aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = imm_receptor_Esmaeil),
    size = 0.2, alpha = 0.8
  ) +
  scale_color_manual(values = receptor_colors, labels = receptor_labels) +
  labs(
    title = "Immune receptors distribution After Data Cleaning",
    x = "UMAP 1",
    y = "UMAP 2"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Separate plot for extracting legend with custom title
legend_plot <- ggplot() +
  geom_point(
    data = full_metadata %>% filter(imm_receptor_Esmaeil != ""),
    aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = imm_receptor_Esmaeil)
  ) +
  scale_color_manual(values = receptor_colors, labels = receptor_labels) +
  labs(color = "immune receptor") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme_void()

legend <- cowplot::get_legend(legend_plot)

# Combine plot and legend side by side
final_plot <- cowplot::plot_grid(main_plot, legend, ncol = 2, rel_widths = c(1, 0.25))

ggsave("UMAP.png", plot = final_plot, width = 8, height = 6, dpi = 600, bg = "white")
ggsave("UMAP.pdf", plot = final_plot, width = 8, height = 6, device = cairo_pdf, bg = "white")