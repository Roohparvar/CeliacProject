library(dplyr)
library(writexl)

# Function to calculate shared clones between pairs of a grouping column
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
      Diagnosis1 = group1,
      Diagnosis2 = group2,
      Shared_Clone_Count = length(shared_clones),
      Shared_Clones = if(length(shared_clones) > 0) paste(shared_clones, collapse = ", ") else ""
    )
  })
  
  bind_rows(results)
}

# Apply the function to the Diagnosis column
shared_diagnosis <- shared_clones_between_groups(full_metadata, "Diagnosis")

# Write to Excel
write_xlsx(
  list(Diagnosis_Shared_Clones = shared_diagnosis),
  path = "Shared_Clones_Between_Diagnosis.xlsx"
)
