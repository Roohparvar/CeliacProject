library(dplyr)
library(writexl)
library(ggplot2)

#-------------------------------------------------------------------------------

output_df <- full_metadata %>%
  select(
    UMAP_1 = scVI_with_hvg_UMAP_1,
    UMAP_2 = scVI_with_hvg_UMAP_2,
    imm_receptor_Jerome,
    cluster
  ) %>%
  mutate(
    color = ifelse(imm_receptor_Jerome == "T and B", "blue", "gray")
  )

write_xlsx(output_df, "Ambiguous_TandB_Distribution.xlsx")

#-------------------------------------------------------------------------------

bar_data <- output_df %>%
  filter(imm_receptor_Jerome == "T and B") %>%
  count(cluster) %>%
  arrange(desc(n)) %>%
  mutate(cluster = factor(cluster, levels = cluster)) 

png("T_and_B_cells_per_cluster.png", width = 2000, height = 1200, res = 300)

ggplot(bar_data, aes(x = cluster, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Count of 'T and B' Cells per Cluster",
    x = "Cluster",
    y = "Number of 'T and B' Cells"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
    axis.text.y = element_text(size = 5),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10)
  )

dev.off()
