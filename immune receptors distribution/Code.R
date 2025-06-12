filtered_data <- full_metadata[!is.na(full_metadata$imm_receptor2) & full_metadata$imm_receptor2 != "", ]

table_data <- table(filtered_data$cluster, filtered_data$imm_receptor2)
count_df <- as.data.frame.matrix(table_data)

receptor_types <- colnames(count_df)

for (receptor in receptor_types) {
  png_filename <- paste0("BarPlot_", gsub(" ", "_", receptor), ".png")
  
  max_y <- max(count_df[[receptor]])
  ylim_max <- max_y * 1.1
  
  png(filename = png_filename, width = 4000, height = 2000, res = 600)
  
  barplot(count_df[[receptor]],
          names.arg = rownames(count_df),
          main = paste("Distribution of", receptor, "across clusters"),
          xlab = "Clusters",
          ylab = "Count",
          col = "steelblue",
          las = 2,
          cex.names = 0.3,
          cex.axis = 0.3,
          ylim = c(0, ylim_max))
  
  dev.off()
}
