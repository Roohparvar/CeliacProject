setwd("C:/Esmaeil/CeliacProject/CeliacProject/4_Clonal Analysis Including Diversity, Expansion, and Clone Size Distribution/Distribution of Clones by Size on UMAP/Clone size of only gamma invariant cells/γδ")


#------------------------------------------------------------------------------- libraries
library(ggplot2)
library(scales)
library(dplyr)
library(patchwork)



# Step 1: Select the subset of cells
subset_cells <- full_metadata %>%
  filter(
    imm_receptor_Esmaeil == "Aberrant g",
    !is.na(g_cdr3) & g_cdr3 != "",
    (is.na(d_cdr3) | d_cdr3 == ""),
    (is.na(cdr_Full_gd) | cdr_Full_gd == "")
  )

# Step 2: Compute clone size within this subset
subset_cells <- subset_cells %>%
  group_by(g_cdr3) %>%
  mutate(clone_size_gd = n()) %>%
  ungroup()

full_metadata$clone_size_gd <- NA

# Step 3: Update clone_size_gd in full_metadata based on subset_cells
full_metadata$clone_size_gd[match(subset_cells$CellID, full_metadata$CellID)] <- subset_cells$clone_size_gd



#------------------------------------------------------------------------------- Individual plots generation and saving
# Data preparation
plot_data <- full_metadata
plot_data$clone_size_gd <- as.numeric(as.character(plot_data$clone_size_gd))

# List of diagnoses (custom order for 2x2 layout)
order_diag <- c("Healthy", "ACD", "RCD-I", "RCD-II")

# List to store individual plots
p_list <- list()

# Loop through each diagnosis
for (diag in order_diag) {
  
  # Subset cells for the current diagnosis
  data_diag <- filter(plot_data, Diagnosis == diag)
  
  # Select only colored cells for this diagnosis (sorted → largest on top)
  data_colored <- data_diag %>%
    filter(!is.na(clone_size_gd)) %>%
    arrange(clone_size_gd)
  

  p <- ggplot() +
    # Layer 1: all cells as gray background
    geom_point(
      data = plot_data,
      aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2),
      color = "grey90",
      size = 0.5,
      alpha = 0.5
    ) +
    # Layer 2: colored cells for this diagnosis (sorted → largest on top)
    geom_point(
      data = data_colored,
      aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = clone_size_gd),
      size = 0.8,
      alpha = 0.9
    ) +
    scale_color_viridis_c(
      name = "Clone size",
      option = "viridis",
      limits = c(1, 33),
      oob = scales::squish,
      labels = function(x) {
        x <- prettyNum(x, digits = 2)
        x[length(x)] <- paste0("≥ ", round(33, 0))
        x
      }
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      legend.title = element_text(vjust = 2),
      legend.title.align = 0.5,
      plot.title = element_text(hjust = 0.5),
      
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    ) +
    labs(
      x = "",
      y = "",
      title = diag
    )
  
  
  pdf_filename <- paste0("UMAP_highlight_", diag, ".pdf")
  png_filename <- paste0("UMAP_highlight_", diag, ".png")
  ggsave(pdf_filename, plot = p, width = 7, height = 6, bg = "white")
  ggsave(png_filename, plot = p, width = 7, height = 6, dpi = 300, bg = "white")
  
  # Store in list for combined plot
  p_list[[diag]] <- p
}


#------------------------------------------------------------------------------- Combining plots and saving the final layout
# Combine the 4 plots into a 2x2 layout
final_plot <- (p_list[["Healthy"]] | p_list[["ACD"]]) /
  (p_list[["RCD-I"]] | p_list[["RCD-II"]]) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right",
        legend.justification = "center")  

ggsave("UMAP_4_diseases.png", plot = final_plot, width = 14, height = 12, dpi = 300, bg = "white")
ggsave("UMAP_4_diseases.pdf", plot = final_plot, width = 14, height = 12, bg = "white")
