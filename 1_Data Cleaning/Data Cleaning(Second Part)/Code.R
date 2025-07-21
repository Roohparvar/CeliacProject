#--------------------------------------------------------------------------------- libraries
library(dplyr)
library(ggplot2)
library(readxl)
library(writexl)
library(Matrix)
library(tidyverse)



abgd_cells <- full_metadata[!is.na(full_metadata$cdr_Full_ab) & full_metadata$cdr_Full_ab != "" &
                              !is.na(full_metadata$cdr_Full_gd) & full_metadata$cdr_Full_gd != "", ]
main_keys <- abgd_cells$FolderName
sub_keys <- gsub("[/-]", ".", abgd_cells$CellID)



# Update CellID names by matching with rownames in list_normalised_gene
sub_keys_updated <- sub_keys

# Loop through each main_key (FolderName) and sub_key (CellID)
for (i in seq_along(main_keys)) {
  main_key <- main_keys[i]
  sub_key <- sub_keys[i]
  
  # Check if main_key exists in list_normalised_gene
  if (main_key %in% names(list_normalised_gene)) {
    df <- list_normalised_gene[[main_key]]
    
    rn <- tryCatch(rownames(df), error = function(e) NULL)  # Safely get rownames
    
    if (!is.null(rn)) {
      # If sub_key matches a rowname, keep it
      if (sub_key %in% rn) {
        cat(sprintf("✅ Main key '%s' exists and sub key '%s' found in rownames.\n", main_key, sub_key))
        
      } else {
        # Try prepending 'X' to sub_key (sometimes needed due to name formatting)
        new_sub_key <- paste0("X", sub_key)
        if (new_sub_key %in% rn) {
          cat(sprintf("🔁 Sub key '%s' not found, but '%s' was found. Updating CellID.\n", sub_key, new_sub_key))
          sub_keys_updated[i] <- new_sub_key  # Update to new matching key
        } else {
          cat(sprintf("⚠️ Main key '%s' exists but neither '%s' nor '%s' found in rownames.\n", main_key, sub_key, new_sub_key))
        }
      }
    } else {
      cat(sprintf("❌ Main key '%s' exists but has no rownames.\n", main_key))
    }
    
  } else {
    cat(sprintf("❌ Main key '%s' NOT found in list.\n", main_key))
  }
}


abgd_cells$CellID <- sub_keys_updated
main_keys <- abgd_cells$FolderName
sub_keys <- abgd_cells$CellID




target_columns <- tolower(c("Trac", "trbc1", "trbc2", "trgc1", "trgc2", "trdc"))

row_list <- list()

for (i in seq_along(main_keys)) {
  main_key <- main_keys[i]
  sub_key <- sub_keys[i]
  
  if (main_key %in% names(list_normalised_gene)) {
    df <- list_normalised_gene[[main_key]]
    
    rn <- tryCatch(rownames(df), error = function(e) NULL)
    
    if (!is.null(rn) && sub_key %in% rn) {
      row_vector <- as.matrix(df[sub_key, , drop = FALSE])
      
      actual_colnames <- tolower(colnames(row_vector))
      
      row_values <- c()
      
      for (col in target_columns) {
        if (col %in% actual_colnames) {
          match_index <- which(actual_colnames == col)
          row_values[col] <- row_vector[1, match_index]
        } else {
          row_values[col] <- 0
        }
      }
      
      row_values <- c(SubKey = sub_key, row_values)
      row_list[[length(row_list) + 1]] <- row_values
    }
  }
}

final_df <- as.data.frame(do.call(rbind, row_list), stringsAsFactors = FALSE)
cols_to_convert <- setdiff(names(final_df), "SubKey")
final_df[cols_to_convert] <- lapply(final_df[cols_to_convert], as.numeric)
final_df <- final_df[order(-final_df$trac), ]


final_df$imm_receptor <- ifelse(
  final_df$trgc1 == 0 & final_df$trgc2 == 0 & final_df$trdc == 0, "ab",
  ifelse(
    final_df$trac == 0 & final_df$trbc1 == 0 & final_df$trbc2 == 0, "gd",
    NA  
  )
)


SSS <- final_df[is.na(final_df$imm_receptor) | final_df$imm_receptor == "" , ]


df <- SSS
df$imm_receptor = NULL
df$SubKey <- factor(df$SubKey, levels = rev(unique(df$SubKey)))

df_long <- df %>%
  pivot_longer(
    cols = -SubKey,
    names_to = "Gene",
    values_to = "Expression"
  )

df_long$Expression <- as.numeric(df_long$Expression)

p <- ggplot(df_long, aes(x = Gene, y = SubKey)) +
  geom_point(aes(size = Expression, color = Expression)) +
  scale_color_viridis_c() +
  scale_size_continuous(range = c(1, 6)) +
  labs(title = "Dot Plot of Gene Expression",
       x = "Gene",
       y = "Cell (SubKey)") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 6))

ggsave("Dotplot_gene_expression.pdf", plot = p, width = 8, height = 49, dpi = 300)