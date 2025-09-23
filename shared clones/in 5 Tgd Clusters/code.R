library(dplyr)
library(tidyr)
library(UpSetR)
library(tibble)


full_metadata = full_metadata[full_metadata$cluster == "Tgd" |
                                full_metadata$cluster == "Tgd CD8+" |
                                full_metadata$cluster == "NK Tgd" |
                                full_metadata$cluster == "Tgd INSIG1+" |
                                full_metadata$cluster == "Trm IEL", ]

full_metadata <- full_metadata %>% filter( !is.na(full_metadata$TRGV) & !is.na(full_metadata$TRGV) )



# Create a named list of clones per cluster
clones_by_cluster <- full_metadata %>%
  filter(!is.na(cdr_Full_gd)) %>%      # remove NAs
  group_by(cluster) %>%
  summarise(clones = list(unique(cdr_Full_gd)))

clones_list <- setNames(clones_by_cluster$clones, clones_by_cluster$cluster)

# Convert to UpSet input
upset_data <- fromList(clones_list)

# Save as PNG
png("shared_clones_Tgd_Clusters.png", width = 2000, height = 2300, res = 300, bg = "white")

upset(upset_data,
      nsets = length(clones_list),
      nintersects = 40,
      order.by = "freq",
      mainbar.y.label = "Shared Clones",
      sets.x.label = "Clones per Tγδ Clusters")

dev.off()



# Save as PDF
pdf("shared_clones_Tgd_Clusters.pdf", width = 10, height = 12)

upset(upset_data,
      nsets = length(clones_list),
      nintersects = 40,
      order.by = "freq",
      mainbar.y.label = "Shared Clones",
      sets.x.label = "Clones per Tγδ Clusters")

dev.off()








get_exact_shared_metadata <- function(metadata, clones_list, clusters) {
  common <- Reduce(intersect, clones_list[clusters])
  
  exact <- common[sapply(common, function(cl) {
    which_sets <- names(which(sapply(clones_list, function(clst) cl %in% clst)))
    setequal(sort(which_sets), sort(clusters))
  })]
  
  result_meta <- metadata %>% filter(cdr_Full_gd %in% exact)
  
  return(result_meta)
}


new_meta <- get_exact_shared_metadata(full_metadata, clones_list, 
                                      c("Trm IEL", "Tgd CD8+"))