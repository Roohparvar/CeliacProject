#-------------------------------------------------------------------------------- Add UMAP plot colored by imm_receptor_Esmaeil with custom colors
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
  "ab" = "TCR⍺β",
  "gd" = "TCRγδ",
  "hkl" = "hkl",
  "Aberrant ab" = "Aberrant ⍺β",
  "Aberrant g" = "Aberrant γ"
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
    title = "Distributions of Cells Colored by Immune Receptor",
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

# Extract the legend object
legend <- cowplot::get_legend(legend_plot)

# Combine plot and legend side by side
final_plot <- cowplot::plot_grid(main_plot, legend, ncol = 2, rel_widths = c(1, 0.25))

# Save image: total width = 10in, height = 6in
ggsave("Umap_imm_receptor_highlighted.png", plot = final_plot, width = 10, height = 6, dpi = 300, bg = "white")




#-------------------------------------------------------------------------------- Bar Plot of Immune Receptor Distribution Across Clusters
valid_receptors <- c("ab", "gd", "hkl", "Aberrant ab", "Aberrant g")


full_metadata <- full_metadata %>%
  filter(
    !is.na(imm_receptor_Esmaeil),
    imm_receptor_Esmaeil %in% valid_receptors,
    !is.na(cluster),
    cluster != ""
  )


table_data <- table(full_metadata$cluster, full_metadata$imm_receptor_Esmaeil)
count_df <- as.data.frame.matrix(table_data)


count_df <- count_df[, colnames(count_df) %in% valid_receptors]


barplot_labels <- c(
  "ab" = "TCRαβ",
  "gd" = "TCRγδ",
  "hkl" = "hkl",
  "Aberrant ab" = "Aberrant αβ",
  "Aberrant g" = "Aberrant γ"
)


for (receptor in colnames(count_df)) {
  png_filename <- paste0("BarPlot_", gsub(" ", "_", receptor), ".png")
  
  max_y <- max(count_df[[receptor]])
  ylim_max <- max_y * 1.1
  
  png(filename = png_filename, width = 4000, height = 2000, res = 600)
  
  barplot(count_df[[receptor]],
          names.arg = rownames(count_df),
          main = paste("Distribution of", barplot_labels[[receptor]], "receptor across clusters"),
          xlab = "Clusters",
          ylab = "Count",
          col = "#984EA3",
          las = 2,
          cex.names = 0.3,
          cex.axis = 0.3,
          ylim = c(0, ylim_max))
  
  dev.off()
}
