library(dplyr)
library(tidyr)
library(UpSetR)
library(tibble)



target_clusters <- c(
  "Plasma cells_1", "B cells_1", "B cells_2",
  "B cells MZB1+", "Plasma cells_2", "Plasmablast",
  "B cells BAFFR", "Tgd INSIG1+", "Tgd", "Tgd CD8+", "ILC2/ILC3", "ILC2/ILTi", "ILC1"
)

full_metadata <- full_metadata %>%
  filter(!cluster %in% target_clusters)



full_metadata <- full_metadata %>% filter( !is.na(full_metadata$TRAV) & !is.na(full_metadata$TRBV) )


# Create a named list of clones per cluster
clones_by_cluster <- full_metadata %>%
  filter(!is.na(cdr_Full_ab)) %>%      # remove NAs
  group_by(cluster) %>%
  summarise(clones = list(unique(cdr_Full_ab)))


clones_list <- setNames(clones_by_cluster$clones, clones_by_cluster$cluster)

# Convert to UpSet input
upset_data <- fromList(clones_list)

# Save as PNG
png("shared_clones_Tcell_Clusters Version 1.png", width = 8000, height = 2300, res = 300, bg = "white")

upset(upset_data,
      nsets = length(clones_list),
      nintersects = 100,
      order.by = "freq",
      mainbar.y.label = "Shared Clones",
      sets.x.label = "Clones per Tcell Clusters")

dev.off()

# Save as PDF
pdf("shared_clones_Tcell_Clusters Version 1.pdf", width = 40, height = 12)

upset(upset_data,
      nsets = length(clones_list),
      nintersects = 100,
      order.by = "freq",
      mainbar.y.label = "Shared Clones",
      sets.x.label = "Clones per Tcell Clusters")

dev.off()


# Save as PNG
png("shared_clones_Tcell_Clusters Version 2.png", width = 8000, height = 2300, res = 300, bg = "white")

upset(upset_data,
      nsets = length(clones_list),
      nintersects = 50,
      order.by = "freq",
      mainbar.y.label = "Shared Clones",
      sets.x.label = "Clones per Tcell Clusters")

dev.off()

# Save as PDF
pdf("shared_clones_Tcell_Clusters Version 2.pdf", width = 40, height = 12)

upset(upset_data,
      nsets = length(clones_list),
      nintersects = 50,
      order.by = "freq",
      mainbar.y.label = "Shared Clones",
      sets.x.label = "Clones per Tcell Clusters")

dev.off()

















get_exact_shared_metadata <- function(metadata, clones_list, clusters) {
  common <- Reduce(intersect, clones_list[clusters])
  
  exact <- common[sapply(common, function(cl) {
    which_sets <- names(which(sapply(clones_list, function(clst) cl %in% clst)))
    setequal(sort(which_sets), sort(clusters))
  })]
  

  result_meta <- metadata %>% filter(cdr_Full_ab %in% exact)
  
  return(result_meta)
}


new_meta <- get_exact_shared_metadata(full_metadata, clones_list, 
                                      c("Th17", "Th"))

