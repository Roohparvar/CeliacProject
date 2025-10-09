setwd("C:/Esmaeil/CeliacProject/CeliacProject/2_immune receptors distribution/immune receptors distribution Before Data Cleaning")


#------------------------------------------------------------------------------- Libraries
library(ggplot2)
library(dplyr)
library(cowplot)



# Define receptor colors and labels
receptor_colors <- c(
  "ab" = "#ee1819",
  "gd" = "#fd7d00",
  "abgd" = "#92509f",
  "T and B" = "#9b592d",
  "hkl" = "#fcd919",
  "Aberant ab" = "#3a78ce",
  "Aberrant g" = "#47ad45",
  "None" = "#eeeeee"
)

receptor_labels <- c(
  "ab" = expression(TCR*alpha*beta),
  "gd" = expression(TRR*gamma*delta),
  "abgd" = expression(TCR*alpha*beta*gamma*delta),
  "T and B" = "T and B",
  "hkl" = "hkl",
  "Aberrant ab" = expression(Aberrant*alpha*beta),
  "Aberrant g" = expression(Aberrant*gamma)
)

# Factor levels
full_metadata$imm_receptor <- factor(
  full_metadata$imm_receptor,
  levels = c("ab", "gd", "Aberant ab", "Aberrant g", "hkl", "abgd", "T and B", "None")
)

# Plot without legend
main_plot <- ggplot() +
  geom_point(
    data = full_metadata %>% filter(imm_receptor != ""),
    aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = imm_receptor),
    size = 0.2, alpha = 0.8
  ) +
  scale_color_manual(values = receptor_colors, labels = receptor_labels) +
  labs(
    title = "Immune receptors distribution Before Data Cleaning",
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

# Extract legend with custom title
legend_plot <- ggplot() +
  geom_point(
    data = full_metadata %>% filter(imm_receptor != ""),
    aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = imm_receptor)
  ) +
  scale_color_manual(values = receptor_colors, labels = receptor_labels) +
  labs(color = "immune receptor") + 
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme_void()

legend <- cowplot::get_legend(legend_plot)

# Combine plot + legend 
final_plot <- cowplot::plot_grid(main_plot, legend, ncol = 2, rel_widths = c(1, 0.25))

# Save image
ggsave("UMAP.png", plot = final_plot, width = 8, height = 6, dpi = 600, bg = "white")

ggsave("UMAP.pdf", plot = final_plot, width = 8, height = 6, device = cairo_pdf, bg = "white")