library(dplyr)
library(tidyr)
library(UpSetR)
library(tibble)


full_metadata = full_metadata[full_metadata$imm_receptor_Esmaeil == "Aberrant g" 
                              & !is.na(full_metadata$imm_receptor_Esmaeil), ]

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
png("shared_clones_upset1.png", width = 2000, height = 2300, res = 300, bg = "white")

upset(upset_data,
      nsets = length(clones_list),
      nintersects = 20,
      order.by = "freq",
      mainbar.y.label = "Shared Clones",
      sets.x.label = "Clones per Cluster")

dev.off()

















library(dplyr)
library(tidyr)
library(UpSetR)
library(purrr)

# Prepare clones per cluster (remove NAs)
clones_by_cluster <- full_metadata %>%
  filter(!is.na(cdr_Full_ab)) %>%
  group_by(cluster) %>%
  summarise(clones = list(unique(cdr_Full_ab)))

clones_list <- setNames(clones_by_cluster$clones, clones_by_cluster$cluster)

# Function to get shared clones for any combination of clusters
get_shared_clones <- function(clusters) {
  Reduce(intersect, clones_list[clusters])
}

# Example: get shared clones between Th and Tregs
shared_Th_Tregs <- get_shared_clones(c("Th", "Tregs"))
print(shared_Th_Tregs)

# ---- Now build ALL intersections ----
cluster_names <- names(clones_list)

# Generate all combinations of clusters
all_combos <- unlist(lapply(1:length(cluster_names), function(i) {
  combn(cluster_names, i, simplify = FALSE)
}), recursive = FALSE)

# Build a list of shared clonotypes for each bar
shared_clones_all <- map(all_combos, get_shared_clones)

# Combine into a tidy data frame
shared_table <- tibble(
  clusters = map_chr(all_combos, ~ paste(.x, collapse = " & ")),
  shared_clones = shared_clones_all,
  n_shared = map_int(shared_clones_all, length)
)

# View table (which corresponds to the bars in the UpSet plot)
print(shared_table)
