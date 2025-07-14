filtered_data <- full_metadata[full_metadata$cluster == "CD4-CD8-", ]

# Count the number of cells by immune receptor type
receptor_counts <- table(filtered_data$imm_receptor_Esmaeil)

# Create a pie chart
pie(
  receptor_counts,
  main = "Immune Receptor Types in CD4-CD8- Cluster",
  col = rainbow(length(receptor_counts)),
  radius = 1
)



filtered_data <- full_metadata[full_metadata$imm_receptor_Esmaeil == "" | is.na(full_metadata$imm_receptor_Esmaeil), ]

filtered_data <- filtered_data[(is.na(filtered_data$g_cdr3) & is.na(filtered_data$d_cdr3)) & (!is.na(filtered_data$a_cdr3) & !is.na(filtered_data$b_cdr3)), ]
