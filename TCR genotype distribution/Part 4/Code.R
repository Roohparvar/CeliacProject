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
  ungroup()

# ترتیب cluster طوری که "Trm IEL" اول باشد
all_clusters <- unique(dot_data$cluster)
cluster_order <- c("Trm IEL", setdiff(all_clusters, "Trm IEL"))

dot_data <- dot_data %>%
  mutate(cluster = factor(cluster, levels = cluster_order))



# مرتب‌سازی ژن‌ها بر اساس فراوانی در "Trm IEL" از کوچک به بزرگ
gene_order <- dot_data %>%
  filter(chain == "TRBV", cluster == "Trm IEL") %>%
  arrange(freq) %>%
  pull(gene)

dot_data <- dot_data %>%
  mutate(gene = factor(gene, levels = gene_order))

# حذف NA و رشته‌های خالی از gene
dot_data <- dot_data %>% filter(!is.na(gene) & trimws(gene) != "")


# رسم نمودار TRBV فقط
dot_plot_trbv <- ggplot(filter(dot_data, chain == "TRBV"), aes(x = cluster, y = gene)) +
  geom_point(aes(size = freq, color = chain), alpha = 0.7) +
  scale_size_continuous(name = "Frequency") +
  scale_color_manual(values = c("TRBV" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  labs(x = "Cluster", y = "TCR Gene", title = "TCR V Gene Usage in Tregs (TRBV only)")

# ذخیره تصویر
ggsave("TCR_dotplot_TRBV.png", plot = dot_plot_trbv, width = 8, height = 15, dpi = 300, bg = "white")
