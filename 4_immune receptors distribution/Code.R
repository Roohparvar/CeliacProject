#-------------------------------------------------------------------------------- Add UMAP plot colored by imm_receptor_Esmaeil with custom colors
library(ggplot2)
library(dplyr)

receptor_colors <- c(
  "ab" = "#ee1819",
  "gd" = "#fd7d00",
  "hkl" = "#fcd919",
  "Aberrant ab" = "#3a78ce",
  "Aberrant g" = "#47ad45",
  "None" = "#eeeeee"
)

full_metadata$imm_receptor_Esmaeil <- factor(
  full_metadata$imm_receptor_Esmaeil,
  levels = c("", "ab", "gd", "Aberrant ab", "Aberrant g", "hkl")
)

receptor_labels <- c(
  "ab" = "TCR⍺β",
  "gd" = "TCRγδ",
  "hkl" = "hkl",
  "Aberrant ab" = "Aberrant ⍺β",
  "Aberrant g" = "Aberrant γ"
)

p <- ggplot() +
  geom_point(
    data = full_metadata %>% filter(imm_receptor_Esmaeil == ""),
    aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = imm_receptor_Esmaeil),
    size = 0.2, alpha = 0.5
  ) +
  geom_point(
    data = full_metadata %>% filter(imm_receptor_Esmaeil != ""),
    aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = imm_receptor_Esmaeil),
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
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

ggsave("Umap_imm_receptor_highlighted.png", plot = p, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("Umap_imm_receptor_highlighted.pdf", plot = p, width = 8, height = 6, dpi = 300, bg = "white")

#-------------------------------------------------------------------------------- Bar Plot of Immune Receptor Distribution Across Clusters
valid_receptors <- c("ab", "gd", "hkl", "Aberrant ab", "Aberrant g")
full_metadata <- full_metadata[
  !is.na(full_metadata$imm_receptor_Esmaeil) &
    full_metadata$imm_receptor_Esmaeil %in% valid_receptors, ]

filtered_data <- full_metadata[
  !is.na(full_metadata$cluster) & full_metadata$cluster != "", ]

table_data <- table(filtered_data$cluster, filtered_data$imm_receptor_Esmaeil)
count_df <- as.data.frame.matrix(table_data)

if ("V1" %in% colnames(count_df)) {
  count_df <- count_df[, colnames(count_df) != "V1"]
}

receptor_types <- colnames(count_df)

barplot_labels <- c(
  "ab" = "TCRαβ",
  "gd" = "TCRγδ",
  "hkl" = "hkl",
  "Aberrant ab" = "Aberrant αβ",
  "Aberrant g" = "Aberrant γ"
)

for (receptor in receptor_types) {
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








filtered_data <- full_metadata[full_metadata$cluster == "CD4-CD8-", ]
filtered_data <- filtered_data[filtered_data$imm_receptor_Esmaeil == "" | is.na(filtered_data$imm_receptor_Esmaeil) , ]
filtered_data <- filtered_data[!is.na(filtered_data$g_cdr3) & !is.na(filtered_data$d_cdr3) , ]




