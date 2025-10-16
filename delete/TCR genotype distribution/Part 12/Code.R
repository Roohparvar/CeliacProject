library(dplyr)
library(tidyr)
library(purrr)


full_metadata <- full_metadata[!duplicated(full_metadata$cdr_Full_ab), ]


cluster_order <- c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs")

create_new_matrix <- function(diagnosis_label) {
  patients <- unique(full_metadata$Patient[full_metadata$Diagnosis == diagnosis_label])
  gene_counts_list <- list()
  
  for (pat in patients) {
    filtered_metadata <- full_metadata %>%
      filter(
        !is.na(TRAV) & TRAV != "",
        !is.na(TRBV) & TRBV != "",
        cluster %in% cluster_order,
        Diagnosis == diagnosis_label,
        Patient == pat
      ) %>%
      distinct(cdr_Full_ab, .keep_all = TRUE)
    
    trav_cluster_counts <- filtered_metadata %>%
      filter(TRAV != "") %>%
      group_by(TRAV, cluster) %>%
      summarise(count = n(), .groups = "drop") %>%
      complete(TRAV, cluster = cluster_order, fill = list(count = 0))
    
    trav_wide <- trav_cluster_counts %>%
      pivot_wider(names_from = cluster, values_from = count) %>%
      mutate(All = rowSums(across(all_of(cluster_order)))) %>%
      select(TRAV, All) %>%
      rename(Gene = TRAV, !!pat := All)
    
    trbv_cluster_counts <- filtered_metadata %>%
      filter(TRBV != "") %>%
      group_by(TRBV, cluster) %>%
      summarise(count = n(), .groups = "drop") %>%
      complete(TRBV, cluster = cluster_order, fill = list(count = 0))
    
    trbv_wide <- trbv_cluster_counts %>%
      pivot_wider(names_from = cluster, values_from = count) %>%
      mutate(All = rowSums(across(all_of(cluster_order)))) %>%
      select(TRBV, All) %>%
      rename(Gene = TRBV, !!pat := All)
    
    patient_gene_counts <- bind_rows(trav_wide, trbv_wide)
    gene_counts_list[[pat]] <- patient_gene_counts
  }
  
  merged <- reduce(gene_counts_list, full_join, by = "Gene")
  merged[is.na(merged)] <- 0
  
  # Add total count column
  merged$All <- rowSums(merged[, -1])
  
  num_patients <- length(patients)
  
  # Average count per patient (All / number_of_patients)
  merged$Average_per_patient <- merged$All / num_patients
  
  return(merged)
}

# Usage examples:
new_matrixHealthy <- create_new_matrix("Healthy")
new_matrixACD     <- create_new_matrix("ACD")
new_matrixRCD_I   <- create_new_matrix("RCD-I")
new_matrixRCD_II  <- create_new_matrix("RCD-II")






















add_missing_genes <- function(mat, all_genes) {
  missing_genes <- setdiff(all_genes, mat$Gene)
  if(length(missing_genes) > 0) {
    # Create rows with zero counts for missing genes
    zero_rows <- data.frame(
      Gene = missing_genes,
      matrix(0, nrow = length(missing_genes), ncol = ncol(mat) - 1)
    )
    colnames(zero_rows)[-1] <- colnames(mat)[-1]
    mat <- bind_rows(mat, zero_rows)
  }
  # Sort rows by Gene
  mat <- mat %>% arrange(Gene)
  return(mat)
}

# Get union of all genes from the four matrices
all_genes <- unique(c(
  new_matrixHealthy$Gene,
  new_matrixACD$Gene,
  new_matrixRCD_I$Gene,
  new_matrixRCD_II$Gene
))

# Add missing genes with zeros to each matrix
new_matrixHealthy <- add_missing_genes(new_matrixHealthy, all_genes)
new_matrixACD     <- add_missing_genes(new_matrixACD, all_genes)
new_matrixRCD_I   <- add_missing_genes(new_matrixRCD_I, all_genes)
new_matrixRCD_II  <- add_missing_genes(new_matrixRCD_II, all_genes)

















# تعریف ستون Gene
genes <- new_matrixACD$Gene

# محاسبه log2 fold change با +1
log2fc <- log2( (new_matrixACD$Average_per_patient + 1) / (new_matrixHealthy$Average_per_patient + 1) )

# ساخت دیتافریم اولیه
new_df <- data.frame(Gene = genes, log2FC = log2fc)

# ستون‌هایی که نمیخوایم حذف کنیم
cols_to_remove <- c("Average_per_patient", "All")

# حذف ستون‌ها از ماتریس‌ها
healthy_new <- new_matrixHealthy[, !(colnames(new_matrixHealthy) %in% cols_to_remove)]
acd_new <- new_matrixACD[, !(colnames(new_matrixACD) %in% cols_to_remove)]

# تعداد سطرها
n_rows <- nrow(healthy_new)

# وکتور خالی برای p-value
p_values <- numeric(n_rows)

# انجام t-test برای هر سطر
for (i in 1:n_rows) {
  healthy_values <- as.numeric(healthy_new[i, ])
  acd_values <- as.numeric(acd_new[i, ])
  
  test <- t.test(healthy_values, acd_values)
  p_values[i] <- test$p.value
}

# اضافه کردن ستون p-value به new_df
new_df$P_value <- p_values


