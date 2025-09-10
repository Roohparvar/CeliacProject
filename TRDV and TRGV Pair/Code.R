library(dplyr)


full_metadata = full_metadata[full_metadata$cluster == "Tgd" |
                                full_metadata$cluster == "Tgd CD8+" |
                                full_metadata$cluster == "NK Tgd" |
                                full_metadata$cluster == "Tgd CD8+" , ]


full_metadata <- full_metadata %>% filter( !is.na(full_metadata$TRDV) & !is.na(full_metadata$TRGV) )


result <- full_metadata %>%
  group_by(Diagnosis, TRDV) %>%
  summarise(TRGVs = paste(unique(TRGV), collapse = ","), .groups = "drop") %>%
  arrange(Diagnosis, TRDV)




# install.packages("circlize")
library(circlize)

# Get list of Diagnosiss
Diagnosiss <- unique(full_metadata$Diagnosis)

# Loop over each Diagnosis
for (p in Diagnosiss) {
  
  # Subset the data for this Diagnosis
  df <- subset(full_metadata, Diagnosis == p, select = c("TRDV", "TRGV"))
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
    
    dev.off() 
    circos.clear()  
  }
}








diagnosis_summary <- full_metadata %>%
  group_by(Diagnosis, TRDV) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(total = sum(count),
         percent = round(100 * count / total, 2)) %>%
  ungroup() %>%
  arrange(Diagnosis, TRDV)

print(diagnosis_summary)





library(ggplot2)

# Plot with dark colors
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

# Save as PNG
ggsave("TRDV_by_Diagnosis_dark.png", plot = p, width = 8, height = 6, dpi = 600, bg = "white")




























library(ggplot2)
library(ggalluvial)
library(scales)
library(dplyr)

# Prepare the data
alluvial_data <- diagnosis_summary %>%
  mutate(Diagnosis = factor(Diagnosis),
         TRDV = factor(TRDV)) %>%
  rename(Stage = Diagnosis, id = TRDV, Freq = percent) %>%
  mutate(alluvium = id)  # Needed for geom_flow

# Define colors
trdv_colors <- c(
  "TRDV1" = "#c6ab52",
  "TRDV2" = "#679966",
  "TRDV3" = "#ff6766"
)

# Plot
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

print(p)

# Save as PNG
ggsave("TRDV_alluvial_percentages.png",
       plot = p, width = 10, height = 5, dpi = 300, bg = "white")









































diagnosis_summaryTRGV <- full_metadata %>%
  group_by(Diagnosis, TRGV) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(total = sum(count),
         percent = round(100 * count / total, 2)) %>%
  ungroup() %>%
  arrange(Diagnosis, TRGV)

print(diagnosis_summaryTRGV)



dark_colors <- c(
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

# Plot
p <- ggplot(diagnosis_summaryTRGV, aes(x = Diagnosis, y = percent, fill = TRGV)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Diagnosis", y = "Percentage",
       title = "Proportion of TRGV types per Diagnosis") +
  scale_fill_manual(values = dark_colors) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

# Save as PNG
ggsave("TRGV_by_Diagnosis_dark.png", plot = p, width = 8, height = 6, dpi = 600, bg = "white")






















library(ggplot2)
library(ggalluvial)
library(scales)
library(dplyr)


# Ensure all combinations exist, fill missing with 0
alluvial_data_TRGV <- diagnosis_summaryTRGV %>%
  mutate(Diagnosis = factor(Diagnosis),
         TRGV = factor(TRGV)) %>%
  rename(Stage = Diagnosis, id = TRGV, Freq = percent) %>%
  mutate(alluvium = id) %>%  # Needed for geom_flow
  complete(Stage, id, fill = list(Freq = 0)) %>%  # fill missing with 0
  mutate(alluvium = id)  # alluvium must match id

alluvial_data_TRGV[is.na(alluvial_data_TRGV)] <- 0

# Colors
dark_colors <- c(
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
  scale_fill_manual(values = dark_colors) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Proportion of TRGV Types per Diagnosis",
       x = "Diagnosis",
       y = "Percentage (%)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5))

print(p)

# Save as PNG
ggsave("TRGV_alluvial_percentages_filtered.png",
       plot = p, width = 10, height = 5, dpi = 300, bg = "white")





























library(dplyr)

ResultTRDV <- full_metadata %>%
  group_by(Diagnosis, cluster, TRDV) %>%
  summarise(count = n(), .groups = "drop_last") %>%
  mutate(cluster_total = sum(count),
         percent = round(100 * count / cluster_total, 2)) %>%
  ungroup() %>%
  arrange(Diagnosis, cluster, TRDV)











