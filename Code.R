########################################################### Start | Duplicate the 'imm_receptor' column as 'imm_receptor2'

full_metadata$imm_receptor2 <- full_metadata$imm_receptor

cols <- colnames(full_metadata)
i <- which(cols == "imm_receptor")

new_order <- append(cols, "imm_receptor2", after = i)
new_order <- new_order[!duplicated(new_order)]

full_metadata <- full_metadata[, new_order]

########################################################### End | Duplicate the 'imm_receptor' column as 'imm_receptor2'



########################################################### Start | Remove "gd", Aberant ab" and "Aberrant g" values from imm_receptor2 in selected B cell clusters
target_clusters <- c(
  "Mast cells", "Plasma cells", "B cells_1", "B cells_2", "B cells MZB1+",
  "Aber. Plasma cells", "Macrophages", "Plasmablast", "B cells BAFFR", "Dendritic cells"
)

target_receptors <- c("Aberant ab", "Aberrant g", "gd")

rows_to_clean <- which(
  full_metadata$cluster %in% target_clusters &
    full_metadata$imm_receptor2 %in% target_receptors
)

full_metadata$imm_receptor2[rows_to_clean] <- ""
########################################################### End | Remove "gd", Aberant ab" and "Aberrant g" values from imm_receptor2 in selected B cell clusters



########################################################### Start | Remove hkl values from imm_receptor2 in selected T cell clusters
target_clusters <- c(
  "Mast cells", "Plasma cells", "B cells_1", "B cells_2", "B cells MZB1+",
  "Aber. Plasma cells", "Macrophages", "Plasmablast", "B cells BAFFR", "Dendritic cells"
)

rows_to_clear <- which(
  !(full_metadata$cluster %in% target_clusters) & 
    full_metadata$imm_receptor2 == "hkl"
)


full_metadata$imm_receptor2[rows_to_clear] <- ""
########################################################### End | Remove hkl values from imm_receptor2 in selected T cell clusters
