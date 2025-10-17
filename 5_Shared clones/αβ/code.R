setwd("C:/Esmaeil/CeliacProject/CeliacProject/6_Shared clones/αβ")
library(dplyr)
library(tidyr)
library(UpSetR)
library(tibble)



target_clusters <- c(
  "Homing plasmablast", "Act. plasma IGHA+", "Mature plasma IGHA+",
  "Act. plasmablast", "Plasma IGHG+", "Mem B cells",
  "B cells BAFFR+", "Macrophages", "pDC", "Mast cells", "Tgd INSIG1+", "NK/Tgd", "Act. Tgd", "Tgd CD8+"
)



full_metadata <- full_metadata %>%
  filter(!cluster %in% target_clusters)



full_metadata <- full_metadata %>% filter( !is.na(full_metadata$cdr_Full_ab) & !is.na(full_metadata$cdr_Full_ab) )


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
png("shared_clones_Tcell_Clusters Version 2.png", width = 8000, height = 4500, res = 600, bg = "white")

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
                                      c("CD4 Trm", "CD4 Trm"))

