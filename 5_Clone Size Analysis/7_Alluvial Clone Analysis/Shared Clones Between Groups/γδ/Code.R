library(dplyr)
library(writexl)

# Function to calculate shared γδ clones between pairs of a grouping column
shared_clones_between_groups_gd <- function(df, group_col) {
  groups <- df %>%
    filter(!is.na(.data[[group_col]]), .data[[group_col]] != "") %>%
    distinct(.data[[group_col]]) %>%
    pull()
  
  group_pairs <- combn(groups, 2, simplify = FALSE)
  
  results <- lapply(group_pairs, function(pair) {
    group1 <- pair[1]
    group2 <- pair[2]
    
    clones_group1 <- df %>%
      filter(.data[[group_col]] == group1, !is.na(cdr_Full_gd), cdr_Full_gd != "") %>%
      pull(cdr_Full_gd) %>% unique()
    
    clones_group2 <- df %>%
      filter(.data[[group_col]] == group2, !is.na(cdr_Full_gd), cdr_Full_gd != "") %>%
      pull(cdr_Full_gd) %>% unique()
    
    shared_clones <- intersect(clones_group1, clones_group2)
    
    data.frame(
      Group1 = group1,
      Group2 = group2,
      Shared_Clone_Count = length(shared_clones),
      Shared_Clones = if(length(shared_clones) > 0) paste(shared_clones, collapse = ", ") else ""
    )
  })
  
  bind_rows(results)
}


shared_gd <- shared_clones_between_groups_gd(full_metadata, "Patient")
write_xlsx(
  list(Patient_Shared_Clones_gd = shared_gd),
  path = "Shared_γδ_Clones_Between_Patient.xlsx"
)


shared_gd <- shared_clones_between_groups_gd(full_metadata, "cluster")
write_xlsx(
  list(Patient_Shared_Clones_gd = shared_gd),
  path = "Shared_γδ_Clones_Between_cluster.xlsx"
)


shared_gd <- shared_clones_between_groups_gd(full_metadata, "Diagnosis")
write_xlsx(
  list(Patient_Shared_Clones_gd = shared_gd),
  path = "Shared_γδ_Clones_Between_Diagnosis.xlsx"
)
