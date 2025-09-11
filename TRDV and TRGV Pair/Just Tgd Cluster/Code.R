library(dplyr)
library(circlize)
library(ggplot2)
library(ggalluvial)
library(scales)
library(tidyr)
library(writexl)


full_metadata = full_metadata[full_metadata$cluster == "Tgd", ]


full_metadata <- full_metadata %>% filter( !is.na(full_metadata$TRDV) & !is.na(full_metadata$TRGV) )


result <- full_metadata %>%
  group_by(Diagnosis, TRDV) %>%
  summarise(TRGVs = paste(unique(TRGV), collapse = ","), .groups = "drop") %>%
  arrange(Diagnosis, TRDV)




# chordDiagram
Diagnosiss <- unique(full_metadata$Diagnosis)

for (p in Diagnosiss) {
  
  df <- subset(full_metadata, Diagnosis == p, select = c("TRDV", "TRGV"))
  df <- na.omit(df)
  
  if (nrow(df) > 0) {
    
    ## --- PDF output ---
    pdf(file = paste0("Circos_", p, ".pdf"), width = 8, height = 8)  
    chordDiagram(df, 
                 transparency = 0.5,
                 annotationTrack = "grid",
                 preAllocateTracks = list(track.height = 0.1))
    
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      sector.name = get.cell.meta.data("sector.index")
      circos.text(CELL_META$xcenter, CELL_META$ylim[1], sector.name,
                  facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
    })
    dev.off()  
    circos.clear()
    
    ## --- PNG output ---
    png(file = paste0("Circos_", p, ".png"), width = 2000, height = 2000, res = 300)
    chordDiagram(df, 
                 transparency = 0.5,
                 annotationTrack = "grid",
                 preAllocateTracks = list(track.height = 0.1))
    
    circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
      sector.name = get.cell.meta.data("sector.index")
      circos.text(CELL_META$xcenter, CELL_META$ylim[1], sector.name,
                  facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
    })
    dev.off()
    circos.clear()
  }
}




# Bar PlotTRDV
diagnosis_summary <- full_metadata %>%
  group_by(Diagnosis, TRDV) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(total = sum(count),
         percent = round(100 * count / total, 2)) %>%
  ungroup() %>%
  arrange(Diagnosis, TRDV)


write_xlsx(diagnosis_summary, path = "diagnosis_summary_TRDV.xlsx")


p <- ggplot(diagnosis_summary, aes(x = Diagnosis, y = percent, fill = TRDV)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Diagnosis", y = "Percentage",
       title = "Proportion of TRDV types per Diagnosis") +
  scale_fill_manual(values = c(
    "TRDV1" = "#c6ab52",   # dark navy
    "TRDV2" = "#679966",   # dark grey-purple
    "TRDV3" = "#ff6766"    # very dark blue
  )) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))


ggsave("TRDV_by_Diagnosis.png", plot = p, width = 10, height = 5, dpi = 600, bg = "white")
ggsave("TRDV_by_Diagnosis.pdf", plot = p, width = 10, height = 5, dpi = 600, bg = "white")






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
ggsave("TRDV_alluvial_percentages.pdf", plot = p, width = 10, height = 5, dpi = 300, bg = "white")





# Bar plot TRGV
diagnosis_summaryTRGV <- full_metadata %>%
  group_by(Diagnosis, TRGV) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(total = sum(count),
         percent = round(100 * count / total, 2)) %>%
  ungroup() %>%
  arrange(Diagnosis, TRGV)


write_xlsx(diagnosis_summaryTRGV, path = "diagnosis_summary_TRGV.xlsx")

diagnosis_summaryTRGV$TRGV <- factor(diagnosis_summaryTRGV$TRGV,
                                     levels = c("TRGV2", "TRGV3", "TRGV4", "TRGV5", 
                                                "TRGV5P", "TRGV8", "TRGV9", "TRGV10"))


dark_colors <- c(
  "TRGV2"  = "#ae1f29",
  "TRGV3"  = "#e05b48",
  "TRGV4"  = "#f1a284",
  "TRGV5"  = "#fbdbc3",
  "TRGV5P" = "#f7f7f7",
  "TRGV8"  = "#cce5f6",
  "TRGV9"  = "#8fc3dd",
  "TRGV10" = "#4790be"
)

# Plot
p <- ggplot(diagnosis_summaryTRGV, aes(x = Diagnosis, y = percent, fill = TRGV)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Diagnosis", y = "Percentage",
       title = "Proportion of TRGV types per Diagnosis") +
  scale_fill_manual(values = dark_colors) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

# Save as PNG
ggsave("TRGV_by_Diagnosis.png", plot = p, width = 10, height = 5, dpi = 600, bg = "white")
ggsave("TRGV_by_Diagnosis.pdf", plot = p, width = 10, height = 5, dpi = 600, bg = "white")






# alluvial plot TRGV V1
alluvial_data_TRGV <- diagnosis_summaryTRGV %>%
  mutate(Diagnosis = factor(Diagnosis),
         TRGV = factor(TRGV,
                       levels = c("TRGV2", "TRGV3", "TRGV4",
                                  "TRGV5", "TRGV5P", "TRGV8",
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
  "TRGV5P" = "#f7f7f7",
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
    aes(label = ifelse(Freq >= 1, paste0(round(Freq, 1), "%"), "")),
    size = 2, color = "black"
  ) +
  scale_x_discrete(expand = c(0.1, 0.1)) +
  scale_fill_manual(values = dark_colors,
                    breaks = c("TRGV2", "TRGV3", "TRGV4",
                               "TRGV5", "TRGV5P", "TRGV8",
                               "TRGV9", "TRGV10")) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Proportion of TRGV Types per Diagnosis",
       x = "Diagnosis",
       y = "Percentage (%)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

# Save as PNG and PDF
ggsave("TRGV_alluvial_percentages_V1.png", plot = p, width = 10, height = 5, dpi = 600, bg = "white")
ggsave("TRGV_alluvial_percentages_V1.pdf", plot = p, width = 10, height = 5, dpi = 600, bg = "white")


