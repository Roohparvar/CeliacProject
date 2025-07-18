library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)  # برای fct_reorder

# Desired cluster order
cluster_order <- c("Tregs")
full_metadata$cluster <- factor(full_metadata$cluster, levels = cluster_order)
full_metadata <- full_metadata %>% filter(!is.na(cluster))

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

# Count, normalize, and reorder genes by frequency (low to high)
dot_data <- combined_data %>%
  group_by(cluster, chain, gene) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(cluster, chain) %>%
  mutate(freq = count / sum(count)) %>%
  ungroup() %>%
  group_by(chain) %>%
  mutate(gene = fct_reorder(gene, freq))  # مرتب‌سازی صعودی (از کم به زیاد)

# Function to create dotplot
make_dotplot <- function(data, title) {
  ggplot(data, aes(x = cluster, y = gene)) +
    geom_point(aes(size = freq, color = chain), alpha = 0.7) +
    scale_size_continuous(name = "Frequency") +
    scale_color_manual(values = c("TRAV" = "#1f77b4", "TRBV" = "#ff7f0e")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Cluster", y = "TCR Gene", title = title)
}

# 1. Combined (TRAV + TRBV)
dot_plot_all <- make_dotplot(dot_data, "TCR V Gene Usage Across Tregs (All)")

# 2. TRAV only
dot_plot_trav <- make_dotplot(filter(dot_data, chain == "TRAV"), "TCR V Gene Usage in Tregs (TRAV only)")

# 3. TRBV only
dot_plot_trbv <- make_dotplot(filter(dot_data, chain == "TRBV"), "TCR V Gene Usage in Tregs (TRBV only)")

# Save plots
ggsave("TCR_dotplot_all_Treg.png", plot = dot_plot_all, width = 6, height = 15, dpi = 300, bg = "white")
ggsave("TCR_dotplot_TRAV_Treg.png", plot = dot_plot_trav, width = 6, height = 15, dpi = 300, bg = "white")
ggsave("TCR_dotplot_TRBV_Treg.png", plot = dot_plot_trbv, width = 6, height = 15, dpi = 300, bg = "white")
