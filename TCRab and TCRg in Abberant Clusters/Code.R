library(ggplot2)
library(dplyr)

# Helper to define valid (non-empty and non-NA) values
is_valid <- function(x) {
  !is.na(x) & x != "" & trimws(x) != ""
}

# Define Group 1 (TCR αβγ): has a, b, g — but no d
group1_cells <- full_metadata %>%
  filter(
    is_valid(a_cdr3),
    is_valid(b_cdr3),
    is_valid(g_cdr3),
    !is_valid(d_cdr3)
  )

# Define Group 2 (TCR γ): only g, no a, b, d
group2_cells <- full_metadata %>%
  filter(
    is_valid(g_cdr3),
    !is_valid(a_cdr3),
    !is_valid(b_cdr3),
    !is_valid(d_cdr3)
  )

# Add a group label column
full_metadata$group_label <- "Other"
full_metadata$group_label[rownames(full_metadata) %in% rownames(group1_cells)] <- "TCR αβγ"
full_metadata$group_label[rownames(full_metadata) %in% rownames(group2_cells)] <- "TCR γ"

# Set factor levels so colored groups are on top
full_metadata$group_label <- factor(full_metadata$group_label, levels = c("Other", "TCR αβγ", "TCR γ"))

# Color palette
group_colors <- c("TCR αβγ" = "#264653", "TCR γ" = "#D95F02")  # blue and red

# Plot
p <- ggplot() +
  # Background gray points (not shown in legend)
  geom_point(data = full_metadata %>% filter(group_label == "Other"),
             aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2),
             color = "lightgrey", size = 0.3, alpha = 0.5) +
  # TCR αβγ
  geom_point(data = full_metadata %>% filter(group_label == "TCR αβγ"),
             aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = group_label),
             size = 0.1, alpha = 1) +
  # TCR γ
  geom_point(data = full_metadata %>% filter(group_label == "TCR γ"),
             aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = group_label),
             size = 0.1, alpha = 1) +
  scale_color_manual(values = group_colors) +
  labs(
    x = "UMAP 1", y = "UMAP 2", color = "TCR Group",
    title = "UMAP of Cells Highlighting TCR αβγ and TCR γ Subsets"
  ) +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9),
    legend.position = "right"
  )

# Save high-resolution image
ggsave("UMAP_TCR_Abberant_Subsets.png", p, width = 8, height = 6, dpi = 400, bg = "white")












library(ggplot2)
library(dplyr)

# فقط سلول‌هایی که در گروه‌های مورد نظر هستند
plot_data <- full_metadata %>%
  filter(group_label %in% c("TCR αβγ", "TCR γ")) %>%
  group_by(cluster, group_label) %>%
  summarise(n_cells = n(), .groups = "drop") %>%
  mutate(cluster = factor(cluster))

# رنگ‌ها
group_colors <- c("TCR αβγ" = "#264653", "TCR γ" = "#D95F02")

# بار پلات
p_bar <- ggplot(plot_data, aes(x = cluster, y = n_cells, fill = group_label)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = group_colors) +
  labs(
    x = "Cluster", y = "Number of Cells", fill = "TCR Group",
    title = "Distribution of TCR αβγ and TCR γ Cells Across Clusters"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9)
  )

# ذخیره تصویر با کیفیت بالا
ggsave("BarPlot_TCR_Groups_PerCluster.png", p_bar, width = 10, height = 5, dpi = 400, bg = "white")


















# --- Bar Plot: Only TCR αβγ ---
plot_abg <- plot_data %>%
  filter(group_label == "TCR αβγ") %>%
  arrange(desc(n_cells)) %>%
  mutate(cluster = factor(cluster, levels = unique(cluster)))

p_abg <- ggplot(plot_abg, aes(x = cluster, y = n_cells)) +
  geom_bar(stat = "identity", fill = "#264653") +
  labs(
    x = "Cluster", y = "Number of Cells",
    title = "Distribution of TCR αβγ Cells Across Clusters"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("BarPlot_TCR_abg_PerCluster.png", p_abg, width = 10, height = 5, dpi = 400, bg = "white")


# --- Bar Plot: Only TCR γ ---
plot_g <- plot_data %>%
  filter(group_label == "TCR γ") %>%
  arrange(desc(n_cells)) %>%
  mutate(cluster = factor(cluster, levels = unique(cluster)))

p_g <- ggplot(plot_g, aes(x = cluster, y = n_cells)) +
  geom_bar(stat = "identity", fill = "#D95F02") +
  labs(
    x = "Cluster", y = "Number of Cells",
    title = "Distribution of TCR γ Cells Across Clusters"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("BarPlot_TCR_g_PerCluster.png", p_g, width = 10, height = 5, dpi = 400, bg = "white")


