setwd("C:/Esmaeil/CeliacProject/CeliacProject/4_Clonal Analysis Including Diversity, Expansion, and Clone Size Distribution/Shannon Entropy")
#------------------------------------------------------------------------------- libraries
library(dplyr)
library(ggplot2)
library(vegan)
library(tidyr)



#------------------------------------------------------------------------------- Helper functions
# Function to recode cluster names
recode_clusters <- function(df) {
  df$cluster <- recode(df$cluster,
                       "Tgd INSIG1+" = "Tγδ INSIG1+",
                       "NK/Tgd" = "NK/Tγδ",
                       "Act. Tgd" = "Act. Tγδ",
                       "Tgd CD8+" = "Tγδ CD8+")
  return(df)
}

# Function to calculate diversity indices
calculate_diversity <- function(df, cdr_col) {
  df %>%
    filter(!is.na(.data[[cdr_col]])) %>%
    group_by(Diagnosis, cluster, .data[[cdr_col]]) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(Diagnosis, cluster) %>%
    summarise(
      shannon  = diversity(count, index = "shannon"),
      simpson  = diversity(count, index = "simpson"),
      richness = specnumber(count),
      .groups = "drop"
    ) %>%
    mutate(shannon_norm = shannon / log(richness))
}

# Function to plot and save figures
plot_and_save <- function(df, type_label, width=10, height=5) {
  Diagnosiss <- unique(df$Diagnosis)
  diagnosis_colors <- c(
    "Healthy" = "#D83A8A",
    "ACD"     = "#349C7C",
    "RCD-I"   = "#D0632B",
    "RCD-II"  = "#6E71AD"
  )
  
  custom_theme <- theme_bw() + 
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(size = 15, angle = 45, hjust = 1)
    )
  
  for (p in Diagnosiss) {
    df_diag <- df %>% filter(Diagnosis == p)
    fill_color <- diagnosis_colors[p]
    
    p1 <- ggplot(df_diag, aes(x = cluster, y = shannon)) +
      geom_bar(stat = "identity", fill = fill_color, width = 0.9) +
      labs(title = paste("Shannon Diversity -", type_label, "-", p),
           x = "Cluster", y = "Shannon Index") +
      custom_theme
    
    p2 <- ggplot(df_diag, aes(x = cluster, y = shannon_norm)) +
      geom_bar(stat = "identity", fill = fill_color, width = 0.9) +
      labs(title = paste("Normalized Shannon Diversity -", type_label, "-", p),
           x = "Cluster", y = "Normalized Shannon Index") +
      custom_theme
    
    ggsave(paste0("shannon_diversity_", type_label, "_", p, ".png"), plot = p1,
           width = width, height = height, dpi = 300, bg = "white")
    ggsave(paste0("shannon_norm_diversity_", type_label, "_", p, ".png"), plot = p2,
           width = width, height = height, dpi = 300, bg = "white")
    ggsave(paste0("shannon_diversity_", type_label, "_", p, ".pdf"), plot = p1,
           width = width, height = height, dpi = 300, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
    ggsave(paste0("shannon_norm_diversity_", type_label, "_", p, ".pdf"), plot = p2,
           width = width, height = height, dpi = 300, bg = "white", device = cairo_pdf, family = "Arial Unicode MS")
  }
}


#------------------------------------------------------------------------------- αβ
full_metadata <- recode_clusters(full_metadata)

# Remove target clusters for αβ
target_clusters_ab <- c(
  "Act. plasma IGHA+", "Act. plasmablast", "B cells BAFFR+", "Homing plasmablast",
  "Macrophages", "Mast cells", "Mature plasma IGHA+", "Mem B cells", "Plasma IGHG+", "pDC",
  "NK/Tγδ", "Tγδ CD8+", "Act. Tγδ", "Tγδ INSIG1+",
  "ILC1/ILC2", "ILC2/ILTi", "ILC3"
)
metadata_ab <- full_metadata %>% filter(!cluster %in% target_clusters_ab)

# Cluster order
cluster_order_ab <- c(
  "CD4-CD8-", "T eff. IEL", "Trm IEL", "IEL CCL4+", "nIEL", "Prolif. IEL", "IEL GZMK+", "Cyt. IEL", "CD4-CD8-IL10+",
  "CD8 Mem", "CD8 Trm", "CD4 FTH1+", "Tregs", "CD4 Trm", "Tfh", "Th17", "NK/Tγδ", "Tγδ CD8+", "Act. Tγδ", "Tγδ INSIG1+",
  "ILC1/ILC2", "ILC2/ILTi", "ILC3"
)
metadata_ab$cluster <- factor(metadata_ab$cluster, levels = cluster_order_ab)

# Calculate indices
diversity_ab <- calculate_diversity(metadata_ab, "cdr_Full_ab")

# Plot and save figures
plot_and_save(diversity_ab, "αβ", width = 10, height = 5)


#------------------------------------------------------------------------------- γδ
full_metadata <- recode_clusters(full_metadata)

# Select only target clusters for γδ
target_clusters_gd <- c("Trm IEL", "NK/Tγδ", "Tγδ CD8+", "Act. Tγδ", "Tγδ INSIG1+")
metadata_gd <- full_metadata %>% filter(cluster %in% target_clusters_gd)

# Cluster order
cluster_order_gd <- target_clusters_gd[target_clusters_gd %in% unique(metadata_gd$cluster)]
metadata_gd$cluster <- factor(metadata_gd$cluster, levels = cluster_order_gd)

# Calculate indices
diversity_gd <- calculate_diversity(metadata_gd, "cdr_Full_gd")

# Plot and save figures
plot_and_save(diversity_gd, "γδ", width = 3, height = 4)


#------------------------------------------------------------------------------- Combining αβ and γδ cell diversity datas
# Add cell type
diversity_ab <- diversity_ab %>% mutate(Cell_type = "αβ")
diversity_gd <- diversity_gd %>% mutate(Cell_type = "γδ")

# Combine the two datasets
combined_diversity <- bind_rows(diversity_ab, diversity_gd)

# To ensure, define clusters as factor with correct order

# αβ order
cluster_order_ab <- c(
  "CD4-CD8-", "T eff. IEL", "Trm IEL", "IEL CCL4+", "nIEL", "Prolif. IEL", 
  "IEL GZMK+", "Cyt. IEL", "CD4-CD8-IL10+", "CD8 Mem", "CD8 Trm", 
  "CD4 FTH1+", "Tregs", "CD4 Trm", "Tfh", "Th17", "NK/Tγδ", 
  "Tγδ CD8+", "Act. Tγδ", "Tγδ INSIG1+", "ILC1/ILC2", "ILC2/ILTi", "ILC3"
)

# γδ order
cluster_order_gd <- c("Trm IEL", "NK/Tγδ", "Tγδ CD8+", "Act. Tγδ", "Tγδ INSIG1+")

combined_diversity <- combined_diversity %>%
  mutate(cluster = case_when(
    Cell_type == "αβ" ~ factor(cluster, levels = cluster_order_ab),
    Cell_type == "γδ" ~ factor(cluster, levels = cluster_order_gd)
  ))

# Diagnosis coloring
diagnosis_colors <- c(
  "Healthy" = "#D83A8A",
  "ACD"     = "#349C7C",
  "RCD-I"   = "#D0632B",
  "RCD-II"  = "#6E71AD"
)

# Raw plot
p_raw <- ggplot(combined_diversity, aes(x = cluster, y = shannon, fill = Diagnosis)) +
  geom_bar(stat = "identity", width = 0.9, position = "dodge") +
  facet_grid(Diagnosis ~ Cell_type, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = diagnosis_colors) +
  labs(title = "", x = "", y = "Shannon Index") +
  theme_bw(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 15, angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    strip.text.x = element_text(size = 20, face = "bold"),
    strip.text.y = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16)                  
  )

ggsave("combined_shannon_raw.png", p_raw, width = 15, height = 10, dpi = 300)
ggsave("combined_shannon_raw.pdf", p_raw, width = 15, height = 10, dpi = 300, device = cairo_pdf, family = "Arial Unicode MS")

# Normalized plot
p_norm <- ggplot(combined_diversity, aes(x = cluster, y = shannon_norm, fill = Diagnosis)) +
  geom_bar(stat = "identity", width = 0.9, position = "dodge") +
  facet_grid(Diagnosis ~ Cell_type, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = diagnosis_colors) +
  labs(title = "Normalized Shannon Diversity", x = "", y = "Normalized Shannon Index") +
  theme_bw(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"),
    strip.text.x = element_text(size = 20, face = "bold"),
    strip.text.y = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text  = element_text(size = 16)      
  )

ggsave("combined_shannon_norm.png", p_norm, width = 15, height = 10, dpi = 300)
ggsave("combined_shannon_norm.pdf", p_norm, width = 15, height = 10, dpi = 300, device = cairo_pdf, family = "Arial Unicode MS")
