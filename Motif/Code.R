full_metadata <- full_metadata[
  !is.na(full_metadata$cdr_Full_ab),
]

full_metadata$cdr_Full_ab <- gsub("\\+", "", full_metadata$cdr_Full_ab)


library(stringdist)


cdrs <- unique(full_metadata$cdr_Full_ab)


dist_matrix <- stringdistmatrix(cdrs, cdrs, method = "lv")
dist_matrix <- as.matrix(dist_matrix)


rownames(dist_matrix) <- cdrs
colnames(dist_matrix) <- cdrs
