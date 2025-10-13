library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)


base_dir <- "C:/Esmaeil/CeliacProject/CeliacProject/4_Clonal Expansion Analysis/Based on manually defined clone size ranges/αβ/Cluster by Cluster_Diagnosis by Diagnosis"


diagnoses <- c("Healthy", "ACD", "RCD-I", "RCD-II")
folders <- c("Healthy", "ACD", "RCD1", "RCD2")


clone_colors <- rev(c("#B3B3B3", "#F0CCFF", "#F8766D", "#619CFF", "#F032E6"))
white_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )


all_p1 <- list()
all_p2 <- list()


for (i in seq_along(diagnoses)) {
  
  diagnosis <- diagnoses[i]
  folder <- folders[i]
  outdir <- file.path(base_dir, folder)
  dir.create(outdir, showWarnings = FALSE)
  setwd(outdir)
  
  
  message(paste("Processing:", diagnosis))
  
  
  full_metadata$cluster <- recode(full_metadata$cluster,
                                  "Tgd INSIG1+" = "Tγδ INSIG1+",
                                  "NK/Tgd" = "NK/Tγδ",
                                  "Act. Tgd" = "Act. Tγδ",
                                  "Tgd CD8+" = "Tγδ CD8+")
  
  
  data_filtered <- full_metadata %>%
    filter(Diagnosis == diagnosis)
  
  
  target_clusters <- c(
    "Homing plasmablast", "Act. plasma IGHA+", "Mature plasma IGHA+",
    "Act. plasmablast", "Plasma IGHG+", "Mem B cells",
    "B cells BAFFR+", "Macrophages", "pDC", "Mast cells"
  )
  
  
  
  metadata_filtered <- data_filtered %>%
    filter(!cluster %in% target_clusters) %>%
    mutate(clone_category = case_when(
      clone_size_ab == 1 ~ "Singleton",
      clone_size_ab >= 2 & clone_size_ab <= 10 ~ "Size 2-10",
      clone_size_ab >= 11 & clone_size_ab <= 50 ~ "Size 11-50",
      clone_size_ab >= 51 & clone_size_ab <= 100 ~ "Size 51-100",
      clone_size_ab >= 101 ~ "Size 100+",
      TRUE ~ NA_character_
    ))
  
  
  
  # Define your custom cluster order
  cluster_order <- c(
    "CD4-CD8-", "T eff. IEL", "Trm IEL", "IEL CCL4+", "nIEL", "Prolif. IEL", "IEL GZMK+", "Cyt. IEL", "CD4-CD8-IL10+",
    "CD8 Mem", "CD8 Trm", "CD4 FTH1+", "Tregs", "CD4 Trm", "Tfh", "Th17", "NK/Tγδ", "Tγδ CD8+", "Act. Tγδ", "Tγδ INSIG1+",
    "ILC1/ILC2", "ILC2/ILTi", "ILC3"
  )
  
  # After filtering target clusters and creating df
  metadata_filtered <- metadata_filtered %>%
    mutate(cluster = factor(cluster, levels = cluster_order))  # <--- Set cluster order
  
  
  
  
  df <- metadata_filtered %>%
    filter(!is.na(clone_category)) %>%
    group_by(cluster, clone_category) %>%
    summarise(n_cells = n(), .groups = "drop") %>%
    complete(cluster, clone_category = c("Singleton", "Size 2-10", "Size 11-50", "Size 51-100", "Size 100+"),
             fill = list(n_cells = 0)) %>%
    group_by(cluster) %>%
    mutate(perc = if (sum(n_cells) == 0) 0 else n_cells / sum(n_cells) * 100) %>%
    ungroup()
  
  
  df$clone_category <- factor(df$clone_category,
                              levels = rev(c("Singleton", "Size 2-10", "Size 11-50", "Size 51-100", "Size 100+")))
  

  p1 <- ggplot(df, aes(x = cluster, y = perc, fill = clone_category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = clone_colors) +
    labs(
      x = "Cluster", y = "Percentage of Cells", fill = "Clone Size",
      title = paste(diagnosis, "- Clone Size Percentage")
    ) +
    white_theme
  

  p2 <- ggplot(df, aes(x = cluster, y = n_cells, fill = clone_category)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = clone_colors) +
    labs(
      x = "Cluster", y = "Number of Cells", fill = "Clone Size",
      title = paste(diagnosis, "- Clone Size Raw Counts")
    ) +
    white_theme
  
  
  ggsave("CloneSize_ab_Percentage.png", p1, width = 10, height = 5, dpi = 1800, bg = "white")
  ggsave("CloneSize_ab_Percentage.pdf", p1, width = 10, height = 5, device = cairo_pdf, bg = "white")
  ggsave("CloneSize_ab_RawCounts.png", p2, width = 10, height = 5, dpi = 1800, bg = "white")
  ggsave("CloneSize_ab_RawCounts.pdf", p2, width = 10, height = 5, device = cairo_pdf, bg = "white")
  

  all_p1[[i]] <- p1
  all_p2[[i]] <- p2
}



#------------------------------------------------------------------------------- Merged
library(cowplot)
library(ggplot2)

titles_top <- c("Healthy", "ACD", "RCD1", "RCD2")
titles_bottom <- c("", "", "", "")

# Custom y-axis names for all 8 plots
y_labels_top <- c("Percentage of Cells", "", "", "")
y_labels_bottom <- c("Number of Cells", "", "", "")

# Apply titles and y labels
all_p1_titled <- Map(function(p, t, y) p + ggtitle(t) + ylab(y), all_p1, titles_top, y_labels_top)
all_p2_titled <- Map(function(p, t, y) p + ggtitle(t) + ylab(y), all_p2, titles_bottom, y_labels_bottom)

# Remove x-axis titles ("Clusters" → "")
all_p1_titled <- lapply(all_p1_titled, function(p) p + labs(x = "") + theme(axis.title.x = element_blank()))
all_p2_titled <- lapply(all_p2_titled, function(p) p + labs(x = "") + theme(axis.title.x = element_blank()))

# Extract shared legend from one of the plots
legend <- get_legend(
  all_p1_titled[[1]] +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8)
    )
)

# Remove legends from individual plots
all_p1_titled <- lapply(all_p1_titled, function(p) p + theme(legend.position = "none"))
all_p2_titled <- lapply(all_p2_titled, function(p) p + theme(legend.position = "none"))

# Combine plots (each column = top + bottom)
combined_plots <- plot_grid(
  plot_grid(all_p1_titled[[1]], all_p2_titled[[1]], ncol = 1, labels = c("", "")),
  plot_grid(all_p1_titled[[2]], all_p2_titled[[2]], ncol = 1, labels = c("", "")),
  plot_grid(all_p1_titled[[3]], all_p2_titled[[3]], ncol = 1, labels = c("", "")),
  plot_grid(all_p1_titled[[4]], all_p2_titled[[4]], ncol = 1, labels = c("", "")),
  ncol = 4,
  rel_widths = c(1, 1, 1, 1)
)

# Add shared legend on the right
final_plot <- plot_grid(
  combined_plots, legend,
  ncol = 2,
  rel_widths = c(4, 0.3),
  align = "v"
)


setwd(base_dir)
ggsave("Merged_CloneSize_ab_All.png", final_plot, width = 20, height = 7, dpi = 600, bg = "white")
ggsave("Merged_CloneSize_ab_All.pdf", final_plot, width = 20, height = 7, device = cairo_pdf, bg = "white")