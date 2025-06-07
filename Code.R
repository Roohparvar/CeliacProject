########################################################### Start Duplicate the 'imm_receptor' column as 'imm_receptor2'

full_metadata$imm_receptor2 <- full_metadata$imm_receptor

cols <- colnames(full_metadata)
i <- which(cols == "imm_receptor")

new_order <- append(cols, "imm_receptor2", after = i)
new_order <- new_order[!duplicated(new_order)]

full_metadata <- full_metadata[, new_order]

########################################################### End Duplicate the 'imm_receptor' column as 'imm_receptor2'
