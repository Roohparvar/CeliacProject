library(dbplyr)

# Group 1
DN_CD4CD8_RCD1_Large <- full_metadata[
  full_metadata$cluster == "CD4-CD8-" &
    full_metadata$Diagnosis == "RCD-I" &
    !is.na(full_metadata$clone_size_ab) &
    full_metadata$clone_size_ab > 99,
]


# main_keys and sub_keys # Group 1
main_keys_Large <- DN_CD4CD8_RCD1_Large$FolderName
sub_keys_Large <- gsub("[/-]", ".", as.character(DN_CD4CD8_RCD1_Large$CellID))
sub_keys_Large <- paste0("X", sub_keys_Large)






# ساخت لیست فیلترشده
filtered_list <- list()

# بررسی وجود هر CellID در list_normalised_gene و ذخیره فقط موارد یافت‌شده
for (i in seq_along(sub_keys_Large)) {
  cell_id <- sub_keys_Large[i]
  found <- FALSE
  
  for (main_key in names(list_normalised_gene)) {
    df <- list_normalised_gene[[main_key]]
    rn <- tryCatch(rownames(df), error = function(e) NULL)
    
    if (!is.null(rn) && cell_id %in% rn) {
      # cat("Cell ID", cell_id, "found in:", main_key, "\n")
      found <- TRUE
      
      # استخراج ردیف مورد نظر
      selected_row <- df[cell_id, , drop = FALSE]
      
      # اگر خونه‌ای با همین main_key تو filtered_list قبلاً ساخته شده، اضافه کن
      if (main_key %in% names(filtered_list)) {
        filtered_list[[main_key]] <- rbind(filtered_list[[main_key]], selected_row)
      } else {
        filtered_list[[main_key]] <- selected_row
      }
      break  # بعد از پیدا کردن، نیازی به ادامه نیست
    }
  }
  
  if (!found) {
    cat("Cell ID", cell_id, "was NOT found in any matrix.\n")
  }
}
