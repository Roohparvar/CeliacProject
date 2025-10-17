library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)


# Remove empty ("") or NA values
trav_data <- full_metadata %>%
  filter(!is.na(TRAV) & TRAV != "") %>%
  select(cluster, gene = TRAV)

trbv_data <- full_metadata %>%
  filter(!is.na(TRBV) & TRBV != "") %>%
  select(cluster, gene = TRBV)

# Combine and label
combined_data <- bind_rows(
  trav_data %>% mutate(chain = "TRAV"),
  trbv_data %>% mutate(chain = "TRBV")
)

# Count, normalize, and reorder genes by frequency (from high to low)
dot_data <- combined_data %>%
  group_by(cluster, chain, gene) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(cluster, chain) %>%
  mutate(freq = count / sum(count)) %>%
  ungroup() %>%
  group_by(chain) %>%
  mutate(gene = fct_reorder(gene, freq, .desc = TRUE))




library(ggplot2)
library(rlang)  # برای استفاده از sym()

make_dotplot <- function(data, title, x_var = "gene", y_var = "cluster", x_font_size = 10, y_font_size = 10) {
  ggplot(data, aes(x = !!sym(x_var), y = !!sym(y_var))) +
    geom_point(aes(size = freq, color = chain), alpha = 0.7) +
    scale_size_continuous(name = "Frequency") +
    scale_color_manual(values = c("TRAV" = "#1f77b4", "TRBV" = "#ff7f0e")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = x_font_size),
      axis.text.y = element_text(size = y_font_size),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(x = x_var, y = y_var, title = title)
}



# 1. Combined (TRAV + TRBV) | x = "gene", y = "cluster"
dot_plot_all_XGene <- make_dotplot(dot_data, "TCR V Gene Usage Across Tregs (All)", x_font_size = 7, y_font_size = 10, x_var = "gene", y_var = "cluster")

# 2. TRAV only | x = "gene", y = "cluster"
dot_plot_trav_XGene <- make_dotplot(filter(dot_data, chain == "TRAV"), "TCR V Gene Usage in Tregs (TRAV only)", x_font_size = 8, y_font_size = 10, x_var = "gene", y_var = "cluster")

# 3. TRBV only | x = "gene", y = "cluster"
dot_plot_trbv_XGene <- make_dotplot(filter(dot_data, chain == "TRBV"), "TCR V Gene Usage in Tregs (TRBV only)", x_font_size = 7, y_font_size = 10, x_var = "gene", y_var = "cluster")


# Save plots
ggsave("1_TCR_dotplot_all_XGene.png", plot = dot_plot_all_XGene, width = 20, height = 5, dpi = 300, bg = "white")
ggsave("2_TCR_dotplot_TRAV_XGene.png", plot = dot_plot_trav_XGene, width = 14, height = 5, dpi = 300, bg = "white")
ggsave("3_TCR_dotplot_TRBV_XGene.png", plot = dot_plot_trbv_XGene, width = 16, height = 5, dpi = 300, bg = "white")




# 4. Combined (TRAV + TRBV) | x = "cluster", y = "gene"
dot_plot_all_XCluster <- make_dotplot(dot_data, "TCR V Gene Usage Across Tregs (All)", x_font_size = 8, y_font_size = 12, x_var = "cluster", y_var = "gene")

# 5. TRAV only | x = "cluster", y = "gene"
dot_plot_trav_XCluster <- make_dotplot(filter(dot_data, chain == "TRAV"), "TCR V Gene Usage in Tregs (TRAV only)", x_font_size = 10, y_font_size = 12, x_var = "cluster", y_var = "gene")

# 6. TRBV only | x = "cluster", y = "gene"
dot_plot_trbv_XCluster <- make_dotplot(filter(dot_data, chain == "TRBV"), "TCR V Gene Usage in Tregs (TRBV only)", x_font_size = 10, y_font_size = 12, x_var = "cluster", y_var = "gene")


# Save plots
ggsave("4_TCR_dotplot_all_XCluster.png", plot = dot_plot_all_XCluster, width = 8, height = 20, dpi = 300, bg = "white")
ggsave("5_TCR_dotplot_TRAV_XCluster.png", plot = dot_plot_trav_XCluster, width = 8, height = 14, dpi = 300, bg = "white")
ggsave("6_TCR_dotplot_TRBV_XCluster.png", plot = dot_plot_trbv_XCluster, width = 8, height = 15, dpi = 300, bg = "white")
