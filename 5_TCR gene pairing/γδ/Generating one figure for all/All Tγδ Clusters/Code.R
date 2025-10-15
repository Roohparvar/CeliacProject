setwd("C:/Esmaeil/CeliacProject/CeliacProject/5_TCR gene pairing/γδ/Generating one figure for all/All Tγδ Clusters")
#------------------------------------------------------------------------------- libraries
library(dplyr)
library(circlize)
library(ggplot2)
library(ggalluvial)
library(scales)
library(tidyr)
library(writexl)




full_metadata$cluster <- recode(full_metadata$cluster,
                                "Tgd INSIG1+" = "Tγδ INSIG1+",
                                "NK/Tgd" = "NK/Tγδ",
                                "Act. Tgd" = "Act. Tγδ",
                                "Tgd CD8+" = "Tγδ CD8+"
)


full_metadata = full_metadata[full_metadata$cluster == "Act. Tγδ" |
                                full_metadata$cluster == "Tγδ INSIG1+" |
                                full_metadata$cluster == "NK/Tγδ" |
                                full_metadata$cluster == "Tγδ CD8+" |
                                full_metadata$cluster == "Trm IEL", ]


full_metadata <- full_metadata %>% filter(!is.na(full_metadata$TRDV) & !is.na(full_metadata$TRGV) )


RCD22 = full_metadata[full_metadata$Patient == "RCD2-2",]
RCD21b = full_metadata[full_metadata$Patient == "RCD2-1b",]

table(RCD22$TRDV)
table(RCD21b$TRDV)

result <- full_metadata %>%
  group_by(Diagnosis, TRDV) %>%
  summarise(TRGVs = paste(unique(TRGV), collapse = ","), .groups = "drop") %>%
  arrange(Diagnosis, TRDV)


all_sectors <- unique(c(full_metadata$TRDV, full_metadata$TRGV))
all_sectors <- na.omit(all_sectors)

# Example: define your own colors
custom_colors <- c(
  "TRDV1"  = "#c6ab52", 
  "TRDV2"  = "#679966",  
  "TRDV3"  = "#ff6766",   
  "TRGV2"  = "#ae1f29",
  "TRGV3"  = "#e05b48",
  "TRGV4"  = "#f1a284",
  "TRGV5"  = "#fbdbc3",
  "TRGV5P" = "#f7f7f7",
  "TRGV8"  = "#cce5f6",
  "TRGV9"  = "#8fc3dd",
  "TRGV10" = "#4790be",
  "TRGV11" = "#1666aa"
)

# Assign any sectors not listed above to grey
grid.col <- setNames(rep("grey", length(all_sectors)), all_sectors)
grid.col[names(custom_colors)] <- custom_colors


TRDV_order <- sort(unique(na.omit(full_metadata$TRDV)))

other_sectors <- setdiff(unique(c(full_metadata$TRDV, full_metadata$TRGV)), TRDV_order)
sector_order <- c(TRDV_order, other_sectors)

# chordDiagram
Diagnosiss <- unique(full_metadata$Diagnosis)


for (p in Diagnosiss) {
  
  df <- subset(full_metadata, Diagnosis == p, select = c("TRDV", "TRGV"))
  df <- na.omit(df)
  
  if (nrow(df) > 0) {
    
    png(file = paste0("Circos_", p, ".png"), width = 2300, height = 2280, res = 300)
    chordDiagram(df, 
                 order = sector_order,
                 transparency = 0.5,
                 annotationTrack = "grid",
                 preAllocateTracks = list(track.height = 0.01), grid.col = grid.col)
    
    dev.off()
    circos.clear()
  }
}