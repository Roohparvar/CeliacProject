if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("scRepertoire")
suppressMessages(library(scRepertoire))


target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "DC", "Macrophages", "Mast cells"
)

full_metadata <- full_metadata %>%
  filter(!cluster %in% target_clusters)



metadata_list <- split(full_metadata, full_metadata$cluster)


p <- clonalHomeostasis(metadata_list, cloneCall = "cdr_Full_gd") +
  xlab("Cluster") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1,size = 7),
        axis.text.y = element_text(size = 7))


ggsave("ClonalHomeostasis.png", plot = p, width = 10, height = 4, dpi = 400, units = "in")