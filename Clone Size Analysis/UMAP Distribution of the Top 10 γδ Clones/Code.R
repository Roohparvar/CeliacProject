library(dplyr)
library(ggplot2)


umap_df <- full_metadata %>%
  select(UMAP_1 = scVI_with_hvg_UMAP_1,
         UMAP_2 = scVI_with_hvg_UMAP_2,
         cdr_Full_gd,
         clone_size_gd,
         cluster)


top10_clones <- full_metadata %>%
  group_by(cdr_Full_gd) %>%
  summarise(clone_size = unique(clone_size_gd)[1]) %>%
  arrange(desc(clone_size)) %>%
  slice_head(n = 10) %>%
  pull(cdr_Full_gd)


umap_df <- umap_df %>%
  mutate(group = ifelse(cdr_Full_gd %in% top10_clones, "Top 10 Clones", "Other"))


top10_df <- umap_df %>% filter(group == "Top 10 Clones")
other_df <- umap_df %>% filter(group == "Other")


png("UMAP Distribution of the Top 10 γδ Clones.png", width = 3000, height = 2000, res = 300)
ggplot() +
  geom_point(data = other_df, aes(x = UMAP_1, y = UMAP_2),
             color = "gray80", size = 0.3, alpha = 0.6, show.legend = FALSE) +
  geom_point(data = top10_df, aes(x = UMAP_1, y = UMAP_2, color = group),
             size = 1.2, alpha = 0.9) +
  scale_color_manual(values = c("Top 10 Clones" = "#2171b5")) +
  labs(
    title = "UMAP Distribution of the Top 10 γδ Clones",
    x = "UMAP 1",
    y = "UMAP 2",
    color = "Clone Group"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )
dev.off()
