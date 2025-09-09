library(dplyr)
library(openxlsx)

summary_table <- full_metadata %>%
  group_by(cluster) %>%
  summarise(
    total_cells = n(),  # new column for total number of cells in the cluster
    ab = sum(imm_receptor_Esmaeil == "ab", na.rm = TRUE),
    Aberrant_ab = sum(imm_receptor_Esmaeil == "Aberrant ab", na.rm = TRUE),
    gd = sum(imm_receptor_Esmaeil == "gd", na.rm = TRUE),
    Aberrant_g = sum(imm_receptor_Esmaeil == "Aberrant g", na.rm = TRUE),
    
    only_a = sum(!is.na(a_cdr3) & is.na(b_cdr3) & is.na(g_cdr3) & is.na(d_cdr3)),
    only_b = sum(is.na(a_cdr3) & !is.na(b_cdr3) & is.na(g_cdr3) & is.na(d_cdr3)),
    only_g = sum(is.na(a_cdr3) & is.na(b_cdr3) & !is.na(g_cdr3) & is.na(d_cdr3)),
    only_d = sum(is.na(a_cdr3) & is.na(b_cdr3) & is.na(g_cdr3) & !is.na(d_cdr3)),
    
    a_b = sum(!is.na(a_cdr3) & !is.na(b_cdr3) & is.na(g_cdr3) & is.na(d_cdr3)),
    a_g = sum(!is.na(a_cdr3) & is.na(b_cdr3) & !is.na(g_cdr3) & is.na(d_cdr3)),
    a_d = sum(!is.na(a_cdr3) & is.na(b_cdr3) & is.na(g_cdr3) & !is.na(d_cdr3)),
    b_g = sum(is.na(a_cdr3) & !is.na(b_cdr3) & !is.na(g_cdr3) & is.na(d_cdr3)),
    b_d = sum(is.na(a_cdr3) & !is.na(b_cdr3) & is.na(g_cdr3) & !is.na(d_cdr3)),
    a_b_g = sum(!is.na(a_cdr3) & !is.na(b_cdr3) & !is.na(g_cdr3) & is.na(d_cdr3)),
    a_b_d = sum(!is.na(a_cdr3) & !is.na(b_cdr3) & is.na(g_cdr3) & !is.na(d_cdr3)),
    a_g_d = sum(!is.na(a_cdr3) & is.na(b_cdr3) & !is.na(g_cdr3) & !is.na(d_cdr3)),
    b_g_d = sum(is.na(a_cdr3) & !is.na(b_cdr3) & !is.na(g_cdr3) & !is.na(d_cdr3)),
    
    all_present = sum(!is.na(a_cdr3) & !is.na(b_cdr3) & !is.na(g_cdr3) & !is.na(d_cdr3)),
    all_a_b_g_d_NA = sum(is.na(a_cdr3) & is.na(b_cdr3) & is.na(g_cdr3) & is.na(d_cdr3))
  )

# Save to Excel
write.xlsx(summary_table, "cluster_summary.xlsx")



library(dplyr)
library(openxlsx)

# Remove the cluster column (keep only numeric columns for correlation)
numeric_table <- summary_table %>% select(-cluster)

# Calculate correlation matrix
cor_matrix <- cor(numeric_table, use = "pairwise.complete.obs")

# Convert matrix to data frame and add row names as a column
cor_df <- as.data.frame(cor_matrix)
cor_df <- cbind(Column = rownames(cor_df), cor_df)

# Save to Excel
write.xlsx(cor_df, "cluster_correlation_matrix.xlsx", rowNames = FALSE)

