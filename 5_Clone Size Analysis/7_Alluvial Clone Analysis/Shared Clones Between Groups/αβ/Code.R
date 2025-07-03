library(dplyr)
library(writexl)


shared_clones_between_groups <- function(df, group_col) {
  groups <- df %>% 
    filter(!is.na(.data[[group_col]]), .data[[group_col]] != "") %>% 
    distinct(.data[[group_col]]) %>% 
    pull()
  
  group_pairs <- combn(groups, 2, simplify = FALSE)
  
  results <- lapply(group_pairs, function(pair) {
    group1 <- pair[1]
    group2 <- pair[2]
    
    clones_group1 <- df %>% 
      filter(.data[[group_col]] == group1, !is.na(cdr_Full_ab), cdr_Full_ab != "") %>% 
      pull(cdr_Full_ab) %>% unique()
    
    clones_group2 <- df %>% 
      filter(.data[[group_col]] == group2, !is.na(cdr_Full_ab), cdr_Full_ab != "") %>% 
      pull(cdr_Full_ab) %>% unique()
    
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


shared_diagnosis <- shared_clones_between_groups(full_metadata, "Patient")
write_xlsx(
  list(Diagnosis_Shared_Clones = shared_diagnosis),
  path = "Shared_αβ_Clones_Between_Patient.xlsx"
)


shared_diagnosis <- shared_clones_between_groups(full_metadata, "cluster")
write_xlsx(
  list(Diagnosis_Shared_Clones = shared_diagnosis),
  path = "Shared_αβ_Clones_Between_cluster.xlsx"
)

shared_diagnosis <- shared_clones_between_groups(full_metadata, "Diagnosis")
write_xlsx(
  list(Diagnosis_Shared_Clones = shared_diagnosis),
  path = "Shared_αβ_Clones_Between_Diagnosis.xlsx"
)
