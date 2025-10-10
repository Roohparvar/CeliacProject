# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("scRepertoire")
# suppressMessages(library(scRepertoire))

library(dplyr)
library(tidyr)
library(ggplot2)

# Filter to RCD-I only
full_metadata <- full_metadata[full_metadata$Diagnosis == "RCD-I", ]

# Remove B cells and related
target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)

metadata_filtered <- full_metadata %>%
  mutate(clone_size_gd = as.numeric(as.character(clone_size_gd)),  # convert table to numeric
         clone_size_gd = ifelse(cluster %in% target_clusters, 1, clone_size_gd))

# Define clone size category
metadata_filtered <- metadata_filtered %>%
  mutate(clone_category = case_when(
    clone_size_gd == 1 ~ "Singleton",
    clone_size_gd >= 2 & clone_size_gd <= 10 ~ "Size 2-10",
    clone_size_gd >= 11 & clone_size_gd <= 50 ~ "Size 11-50",
    clone_size_gd >= 51 & clone_size_gd <= 100 ~ "Size 51-100",
    clone_size_gd >= 101 ~ "Size 100+",
    TRUE ~ NA_character_
  ))

# Prepare data for plotting
df <- metadata_filtered %>%
  filter(!is.na(clone_category)) %>%
  group_by(cluster, clone_category) %>%
  summarise(n_cells = n(), .groups = "drop") %>%
  complete(cluster, clone_category = c("Size 100+", "Size 51-100", "Size 11-50", "Size 2-10", "Singleton"),
           fill = list(n_cells = 0)) %>%
  group_by(cluster) %>%
  mutate(perc = if (sum(n_cells) == 0) 0 else n_cells / sum(n_cells) * 100) %>%
  ungroup()

# Set the factor levels in the desired order (from left to right)
df$clone_category <- factor(df$clone_category, levels = rev(c("Size 100+", "Size 51-100", "Size 11-50", "Size 2-10", "Singleton")))

ordered_celltypes <- c(
  "B cells_2", "B cells_1", "Trm IEL", "ILC2/ILTi", "Th17", "DC",
  "CD4-CD8-", "B cells BAFFR", "Plasma cells_2", "Cyt. IEL",
  "CD4-CD8- IL10 ICOS", "IEL GZMK+", "Plasmablast", "Th2/Tfh",
  "B cells MZB1+", "Th1 Mem", "CD8 Trm", "ILC1", "ILC2/ILC3",
  "Tregs", "Mast cells", "Tγδ INSIG1+", "Prolif. IEL", "CD8 Cyt.",
  "Th", "Macrophages", "Plasma cells_1", "NK Tγδ", "Tγδ CD8+",
  "Tγδ", "CD8 Mem", "nIEL", "IEL CCL4+"
)

df$cluster <- factor(df$cluster, levels = ordered_celltypes)
# df$cluster <- factor(df$cluster, levels = rev(ordered_celltypes))
# Plot theme
white_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

# Set colors in matching order
# Matches: Size 100+, Size 51-100, Size 11-50, Size 2-10, Singleton
clone_colors <- rev(c("#1E90FF", "#3A7BD5", "#355C7D", "#2E4057", "#1B263B"))

# Percentage plot (vertical clusters)
p1 <- ggplot(df, aes(y = cluster, x = perc, fill = clone_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = clone_colors) +
  labs(
    y = "Cluster", x = "Percentage of Cells", fill = "TCRγδ Clones",
    title = "γδ Clonal Size Distribution per Cluster in RCD-I (Percentage)"
  ) +
  white_theme +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 0, size = 8))

ggsave("CloneSize_γδ_Percentage_Vertical2.png", p1, width = 6, height = 10, dpi = 400, bg = "white")

# Raw count plot (vertical clusters)
p2 <- ggplot(df, aes(y = cluster, x = n_cells, fill = clone_category)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = clone_colors) +
  labs(
    y = "Cluster", x = "Number of Cells", fill = "TCRγδ Clones",
    title = "γδ Clonal Size Distribution per Cluster in RCD-I (Raw Counts)"
  ) +
  white_theme +
  theme(axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 0, size = 8))

ggsave("CloneSize_γδ_RawCounts_Vertical2.png", p2, width = 6, height = 10, dpi = 400, bg = "white")







# برای ذخیره PDF با فونت Unicode پشتیبانی شده
ggsave("CloneSize_ab_Percentage_Vertical.pdf", p1, width = 6, height = 10, dpi = 400,
       bg = "white", device = cairo_pdf, family = "Arial Unicode MS")

ggsave("CloneSize_ab_RawCounts_Vertical.pdf", p2, width = 6, height = 10, dpi = 400,
       bg = "white", device = cairo_pdf, family = "Arial Unicode MS")