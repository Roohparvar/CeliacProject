library(dplyr)

full_metadata <- full_metadata %>% filter( !is.na(full_metadata$TRDV) & !is.na(full_metadata$TRGV) )





result <- full_metadata %>%
  group_by(PatientName, TRDV) %>%
  summarise(TRGVs = paste(unique(TRGV), collapse = ","), .groups = "drop") %>%
  arrange(PatientName, TRDV)

print(result)








# install.packages("circlize")
library(circlize)

# Get list of patients
patients <- unique(full_metadata$PatientName)

# Loop over each patient
for (p in patients) {
  
  # Subset the data for this patient
  df <- subset(full_metadata, PatientName == p, select = c("TRDV", "TRGV"))
  df <- na.omit(df)
  
  if (nrow(df) > 0) {
    # Save PNG file
    png(filename = paste0("Circos_", p, ".png"), width = 2000, height = 2000, res = 300)
    
    # Draw chord diagram
    chordDiagram(df, 
                 transparency = 0.5,
                 annotationTrack = "grid",
                 preAllocateTracks = list(track.height = 0.1))
    
    # Add labels
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      sector.name = get.cell.meta.data("sector.index")
      circos.text(CELL_META$xcenter, CELL_META$ylim[1], sector.name,
                  facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
    })
    
    dev.off()  # Close the PNG device
    circos.clear()  # Reset circos for next plot
  }
}
