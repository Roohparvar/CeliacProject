setwd("C:/Esmaeil/CeliacProject/CeliacProject/2_immune receptors distribution/immune receptors distribution After Data Cleaning/Bar plot")


#------------------------------------------------------------------------------- libraries
library(ggplot2)
library(dplyr)
library(cowplot)


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



# Define receptor colors and labels
receptor_colors <- c(
  "αβ" = "#ee1819",
  "γδ" = "#fd7d00",
  "hkl" = "#fcd919",
  "Aberrant αβ" = "#3a78ce",
  "Aberrant γ" = "#47ad45",
  "None" = "#eeeeee"
)



#-------------------------------------------------------------------------------- Bar Plot of Immune Receptor Distribution Across Clusters
valid_receptors <- c("αβ", "γδ", "hkl", "Aberrant αβ", "Aberrant γ")


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
  "αβ" = "TCRαβ",
  "γδ" = "TCRγδ",
  "hkl" = "hkl",
  "Aberrant αβ" = "Aberrant αβ",
  "Aberrant γ" = "Aberrant γ"
)


for (receptor in colnames(count_df)) {
  png_filename <- paste0("BarPlot_", gsub(" ", "_", receptor), ".png")
  
  max_y <- max(count_df[[receptor]])
  ylim_max <- max_y * 1.1
  
  png(filename = png_filename, width = 4000, height = 2000, res = 600)
  
  # Draw barplot without axis names
  bp <- barplot(count_df[[receptor]],
                names.arg = FALSE,
                main = paste("Distribution of", barplot_labels[[receptor]], "receptor across clusters"),
                xlab = "",
                ylab = "Count",
                col = receptor_colors[[receptor]],
                cex.main = 1,
                cex.axis = 0.4,
                cex.names = 1.5, # Font size of x labels
                cex.lab = 0.8,
                ylim = c(0, ylim_max))
  
  # Add 45-degree rotated x labels
  text(x = bp, 
       y = par("usr")[3] - 0.05 * ylim_max, 
       labels = rownames(count_df),
       srt = 45,        # rotation angle
       adj = 1,         # right alignment
       xpd = TRUE,      # allow drawing outside plot area
       cex = 0.4)
  
  dev.off()
}




for (receptor in colnames(count_df)) {
  pdf_filename <- paste0("BarPlot_", gsub(" ", "_", receptor), ".pdf")
  
  max_y <- max(count_df[[receptor]])
  ylim_max <- max_y * 1.1
  
  # Open PDF device with Cairo for proper Greek letters
  cairo_pdf(file = pdf_filename, width = 8, height = 5, bg = "white")
  
  # Prepare labels with expression() for Greek letters
  label_expr <- switch(receptor,
                       "αβ" = expression(TCR*alpha*beta),
                       "γδ" = expression(TCR*gamma*delta),
                       "hkl" = "hkl",
                       "Aberrant αβ" = expression(Aberrant*alpha*beta),
                       "Aberrant γ" = expression(Aberrant*gamma))
  
  # Draw barplot
  bp <- barplot(count_df[[receptor]],
                names.arg = FALSE,
                main = bquote("Distribution of " * .(label_expr) * " receptor across clusters"),
                xlab = "",
                ylab = "Count",
                col = receptor_colors[[receptor]],
                cex.main = 1,
                cex.axis = 0.8,
                cex.lab = 0.8,
                ylim = c(0, ylim_max))
  
  # Add 45-degree rotated x labels
  text(x = bp, 
       y = par("usr")[3] - 0.05 * ylim_max, 
       labels = rownames(count_df),
       srt = 45,
       adj = 1,
       xpd = TRUE,
       cex = 0.7)
  
  dev.off()
}
