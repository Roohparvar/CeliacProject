library(dplyr)
library(circlize)
library(ggplot2)
library(ggalluvial)
library(scales)
library(tidyr)
library(writexl)


full_metadata = full_metadata[full_metadata$cluster == "IEL CCL4+" |
                                full_metadata$cluster == "nIEL" |
                                full_metadata$cluster == "Prolif. IEL" |
                                full_metadata$cluster == "IEL GZMK+" , ]


full_metadata = full_metadata[full_metadata$TRAV == "TRAV12-1" |
                                full_metadata$TRAV == "TRAV30" |
                                full_metadata$TRAV == "TRAV12-2" |
                                full_metadata$TRAV == "TRAV8-4" |
                                full_metadata$TRAV == "TRAV34", ]


full_metadata <- full_metadata %>% filter( !is.na(full_metadata$TRAV) & !is.na(full_metadata$TRBV) )


result <- full_metadata %>%
  group_by(Diagnosis, TRAV) %>%
  summarise(TRBVs = paste(unique(TRBV), collapse = ","), .groups = "drop") %>%
  arrange(Diagnosis, TRAV)




all_sectors <- unique(c(full_metadata$TRAV, full_metadata$TRBV))
all_sectors <- na.omit(all_sectors)

# Example: define your own colors
custom_colors <- c(
  "TRAV12-1"  = "#4D648D",   
  "TRAV30"  = "#FDEE00",   
  "TRAV12-2"  = "#997A8D",  
  "TRAV8-4"  = "#FB4D46",
  "TRAV34"  = "#2F847C"
)

# Assign any sectors not listed above to grey
grid.col <- setNames(rep("grey", length(all_sectors)), all_sectors)
grid.col[names(custom_colors)] <- custom_colors


TRAV_order <- sort(unique(na.omit(full_metadata$TRAV)))

other_sectors <- setdiff(unique(c(full_metadata$TRAV, full_metadata$TRBV)), TRAV_order)
sector_order <- c(TRAV_order, other_sectors)

# chordDiagram
Diagnosiss <- unique(full_metadata$Diagnosis)

Diagnosiss <- unique(full_metadata$Diagnosis)

for (p in Diagnosiss) {
  
  df <- subset(full_metadata, Diagnosis == p, select = c("TRAV", "TRBV"))
  df <- na.omit(df)
  
  if (nrow(df) > 0) {
    
    ## --- PDF output ---
    pdf(file = paste0("Circos_", p, ".pdf"), width = 9, height = 9)  
    circos.par(canvas.ylim = c(-0.95, 1))   # shift circle down
    
    chordDiagram(df, 
                 order = sector_order,
                 transparency = 0.5,
                 annotationTrack = "grid",
                 preAllocateTracks = list(track.height = 0.1), 
                 grid.col = grid.col)
    
    title(main = paste("TRAV-TRBV pairing - CD4 Clusters -", p), line = 0.3, cex.main = 0.9)
    
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      sector.name = get.cell.meta.data("sector.index")
      
      # اندازه فونت بر اساس TRAV یا TRBV
      fontsize <- ifelse(grepl("^TRBV", sector.name), 0.5, 0.8) #TRBV: 0.5 and TRAV: 0.8
      
      circos.text(CELL_META$xcenter, CELL_META$ylim[1], sector.name,
                  facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
                  cex = fontsize)
    })
    dev.off()  
    circos.clear()
    
    ## --- PNG output ---
    png(file = paste0("Circos_", p, ".png"), width = 2300, height = 2300, res = 300)
    circos.par(canvas.ylim = c(-1, 1))   # shift circle down
    
    chordDiagram(df, 
                 order = sector_order,
                 transparency = 0.5,
                 annotationTrack = "grid",
                 preAllocateTracks = list(track.height = 0.1), 
                 grid.col = grid.col)
    
    title(main = paste("TRAV-TRBV pairing - CD4 Clusters -", p), line = 0.3, cex.main = 0.9)
    
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      sector.name = get.cell.meta.data("sector.index")
      fontsize <- ifelse(grepl("^TRBV", sector.name), 0.5, 0.8)
      
      circos.text(CELL_META$xcenter, CELL_META$ylim[1], sector.name,
                  facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5),
                  cex = fontsize)
    })
    dev.off()
    circos.clear()
  }
}







# alluvial plot TRAV

diagnosis_summary <- full_metadata %>%
  group_by(Diagnosis, TRAV) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(total = sum(count),
         percent = round(100 * count / total, 2)) %>%
  ungroup() %>%
  arrange(Diagnosis, TRAV)




alluvial_data <- diagnosis_summary %>%
  mutate(Diagnosis = factor(Diagnosis),
         TRAV = factor(TRAV)) %>%
  rename(Stage = Diagnosis, id = TRAV, Freq = percent) %>%
  mutate(alluvium = id)  # Needed for geom_flow



TRAV_colors <- c(
    "TRAV12-1"  = "#4D648D",   
    "TRAV30"  = "#FDEE00",   
    "TRAV12-2"  = "#997A8D",  
    "TRAV8-4"  = "#FB4D46",
    "TRAV34"  = "#2F847C"
)


p <- ggplot(alluvial_data,
            aes(x = Stage, stratum = id, alluvium = alluvium,
                y = Freq, fill = id)) +
  geom_flow(alpha = 0.7, color = "grey50") +
  geom_stratum(width = 0.3, color = "black") +
  geom_text(stat = "stratum", aes(label = paste0(round(Freq, 1), "%")),
            size = 3, color = "black") +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_fill_manual(values = TRAV_colors) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Proportion of TRAV Types per Diagnosis - All Tgd Clusters",
       x = "Diagnosis",
       y = "Percentage (%)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))


# Save as PNG
ggsave("TRAV_alluvial_percentages.png", plot = p, width = 10, height = 5, dpi = 600, bg = "white")
ggsave("TRAV_alluvial_percentages.pdf", plot = p, width = 10, height = 5, dpi = 300, bg = "white")
