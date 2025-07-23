#--------------------------------------------------------------------------------- Add UMAP plot colored by imm_receptor with custom colors
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
  "ab" = "TCR⍺β",
  "gd" = "TRRγδ",
  "abgd" = "⍺βγδ",
  "T and B" = "T and B",
  "hkl" = "hkl",
  "Aberant ab" = "Aberrant ⍺β",
  "Aberrant g" = "Aberrant γ"
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
    title = "UMAP Colored by Immune Receptor",
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

# Combine plot + legend (UMAP = 8in wide, legend = 2in)
final_plot <- cowplot::plot_grid(main_plot, legend, ncol = 2, rel_widths = c(1, 0.25))

# Save image — total width = 10in, height = 6in
ggsave("Umap_imm_receptor_highlighted.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")




#--------------------------------------------------------------------------------- Add UMAP plot colored by cluster with custom colors
full_metadata$cluster <- recode(full_metadata$cluster,
                                "NK Tgd" = "NK Tγδ",
                                "Tgd CD8+" = "Tγδ CD8+",
                                "Tgd INSIG1+" = "Tγδ INSIG1+",
                                "Tgd" = "Tγδ"
)

plot_data <- full_metadata %>%
  filter(!is.na(cluster))  

cluster_centers <- plot_data %>%
  group_by(cluster) %>%
  summarize(x = mean(scVI_with_hvg_UMAP_1),
            y = mean(scVI_with_hvg_UMAP_2))

umap_plot <- ggplot(plot_data, aes(x = scVI_with_hvg_UMAP_1,
                                   y = scVI_with_hvg_UMAP_2,
                                   color = factor(cluster))) +
  geom_point(size = 0.6, alpha = 0.8) +
  geom_text(data = cluster_centers, aes(x = x, y = y, label = cluster),
            color = "black", size = 3, hjust = 0.5, vjust = 0.5) +
  labs(title = "UMAP of Cells Colored by Cluster",
       x = "UMAP 1",
       y = "UMAP 2",
       color = "Cluster") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA))

ggsave("Umap_cluster_highlighted.png", plot = umap_plot, width = 8, height = 6, dpi = 300, bg = "white")