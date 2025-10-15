setwd("C:/Esmaeil/CeliacProject/CeliacProject/5_TCR gene pairing/γδ/Cluster by cluster/Just Tγδ INSIG1+")
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

full_metadata = full_metadata[full_metadata$cluster == "Tγδ INSIG1+" , ]


full_metadata <- full_metadata %>% filter( !is.na(full_metadata$TRDV) & !is.na(full_metadata$TRGV) )


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
  "TRGV8"  = "#cce5f6",
  "TRGV9"  = "#8fc3dd",
  "TRGV10" = "#4790be"
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
    
    ## --- PDF output ---
    cairo_pdf(file = paste0("Circos_", p, ".pdf"), 
              width = 9, height = 9,
              family = "Arial Unicode MS")
    circos.par(canvas.xlim = c(0, 0), canvas.ylim = c(0, 0))
    chordDiagram(df,
                 order = sector_order,
                 transparency = 0.5,
                 annotationTrack = "grid",
                 preAllocateTracks = list(track.height = 0.1), grid.col = grid.col)
    
    title(main = paste("Tγδ INSIG1+ -", p), line = 0, cex.main = 1.5)
    
    circos.trackPlotRegion(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
      sector.name = get.cell.meta.data("sector.index")
      circos.text(CELL_META$xcenter, CELL_META$ylim[1], sector.name,
                  facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
    })
    dev.off()  
    circos.clear()
    
    ## --- PNG output ---
    png(file = paste0("Circos_", p, ".png"), width = 2300, height = 2280, res = 300)
    circos.par(canvas.xlim = c(0, 0), canvas.ylim = c(0, 0))
    chordDiagram(df, 
                 order = sector_order,
                 transparency = 0.5,
                 annotationTrack = "grid",
                 preAllocateTracks = list(track.height = 0.1), grid.col = grid.col)
    
    title(main = paste("Tγδ INSIG1+ -", p), line = 0, cex.main = 1.5)
    
    circos.trackPlotRegion(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
      sector.name = get.cell.meta.data("sector.index")
      circos.text(CELL_META$xcenter, CELL_META$ylim[1], sector.name,
                  facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
    })
    dev.off()
    circos.clear()
  }
}





# diagnosis summary |TRDV
diagnosis_summary <- full_metadata %>%
  group_by(Diagnosis, TRDV) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(total = sum(count),
         percent = round(100 * count / total, 2)) %>%
  ungroup() %>%
  arrange(Diagnosis, TRDV)


write_xlsx(diagnosis_summary, path = "diagnosis_summary_TRDV.xlsx")





# alluvial plot TRDV
alluvial_data <- diagnosis_summary %>%
  mutate(Diagnosis = factor(Diagnosis),
         TRDV = factor(TRDV)) %>%
  rename(Stage = Diagnosis, id = TRDV, Freq = percent) %>%
  mutate(alluvium = id)  # Needed for geom_flow



trdv_colors <- c(
  "TRDV1" = "#c6ab52",
  "TRDV2" = "#679966",
  "TRDV3" = "#ff6766"
)


p <- ggplot(alluvial_data,
            aes(x = Stage, stratum = id, alluvium = alluvium,
                y = Freq, fill = id)) +
  geom_flow(alpha = 0.7, color = "grey50") +
  geom_stratum(width = 0.3, color = "black") +
  geom_text(stat = "stratum", aes(label = paste0(round(Freq, 1), "%")),
            size = 3, color = "black") +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_fill_manual(values = trdv_colors) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Proportion of TRDV Types per Diagnosis",
       x = "Diagnosis",
       y = "Percentage (%)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))


# Save as PNG
ggsave("TRDV_alluvial_percentages.png", plot = p, width = 10, height = 5, dpi = 600, bg = "white")
ggsave("TRDV_alluvial_percentages.pdf", plot = p, width = 10, height = 5, dpi = 300, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")






# diagnosis summary |TRGV
diagnosis_summaryTRGV <- full_metadata %>%
  group_by(Diagnosis, TRGV) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(total = sum(count),
         percent = round(100 * count / total, 2)) %>%
  ungroup() %>%
  arrange(Diagnosis, TRGV)


write_xlsx(diagnosis_summaryTRGV, path = "diagnosis_summary_TRGV.xlsx")







# alluvial plot TRGV V1
alluvial_data_TRGV <- diagnosis_summaryTRGV %>%
  mutate(Diagnosis = factor(Diagnosis),
         TRGV = factor(TRGV,
                       levels = c("TRGV2", "TRGV3", "TRGV4",
                                  "TRGV5", "TRGV8",
                                  "TRGV9", "TRGV10"))) %>%
  rename(Stage = Diagnosis, id = TRGV, Freq = percent) %>%
  mutate(alluvium = id) %>%  # Needed for geom_flow
  complete(Stage, id, fill = list(Freq = 0)) %>%
  mutate(alluvium = id)

# Replace any remaining NA with 0
alluvial_data_TRGV[is.na(alluvial_data_TRGV)] <- 0

# Colors (match order of legend)
dark_colors <- c(
  "TRGV2"  = "#ae1f29",
  "TRGV3"  = "#e05b48",
  "TRGV4"  = "#f1a284",
  "TRGV5"  = "#fbdbc3",
  "TRGV8"  = "#cce5f6",
  "TRGV9"  = "#8fc3dd",
  "TRGV10" = "#4790be"
)

# Plot
p <- ggplot(alluvial_data_TRGV,
            aes(x = Stage, stratum = id, alluvium = alluvium,
                y = Freq, fill = id)) +
  geom_flow(alpha = 0.7, color = "grey50") +
  geom_stratum(width = 0.3, color = "black") +
  geom_text(
    stat = "stratum",
    aes(label = ifelse(Freq >= 2, paste0(round(Freq, 1), "%"), "")),
    size = 2, color = "black"
  ) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_fill_manual(values = dark_colors,
                    breaks = c("TRGV2", "TRGV3", "TRGV4",
                               "TRGV5", "TRGV8",
                               "TRGV9", "TRGV10")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Proportion of TRGV Types per Diagnosis",
       x = "Diagnosis",
       y = "Percentage (%)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

# Save as PNG and PDF
ggsave("TRGV_alluvial_percentages_V1.png", plot = p, width = 10, height = 5, dpi = 600, bg = "white")
ggsave("TRGV_alluvial_percentages_V1.pdf", plot = p, width = 10, height = 5, dpi = 600, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")