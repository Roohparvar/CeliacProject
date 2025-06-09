############################################################ Start | Libraries
library(openxlsx)

library(dplyr)
library(ggplot2)
library(openxlsx)
############################################################ End | Libraries



############################################################ Start | Reorder essential columns needed for TCR analysis
important_cols <- c(
  "a_cdr3", "b_cdr3", "g_cdr3", "d_cdr3",
  "TRAV", "TRAJ", "TRBV", "TRBJ", "TRGV", "TRGJ", "TRDV", "TRDJ",
  "contigCount_T", "clone_size_ab", "clone_size_gd",
  "imm_receptor", "imm_receptor2", "cluster"
)

important_cols <- important_cols[important_cols %in% colnames(full_metadata)]

remaining_cols <- setdiff(colnames(full_metadata), important_cols)
new_order <- c(important_cols, remaining_cols)

full_metadata <- full_metadata[, new_order]

write.xlsx(full_metadata, "MetaData_CLEANED_reorder_essential_columns_for_TCR.xlsx", row.names = FALSE)

if (exists("patient_colours") & exists("diagnosis_colours") & exists("palette_34")) {
  save(full_metadata, patient_colours, diagnosis_colours, palette_34,
       file = "MetaData_CLEANED_reorder_essential_columns_for_TCR.Rdata")
} else {
  save(full_metadata, file = "MetaData_CLEANED_reorder_essential_columns_for_TCR.Rdata")
}
############################################################ End | Reorder essential columns needed for TCR analysis



############################################################ Start | Clone Size Table
clone_table <- full_metadata %>%
  filter(!is.na(a_cdr3), a_cdr3 != "", !is.na(clone_size_ab)) %>%
  select(a_cdr3, clone_size_ab) %>%
  distinct() %>%
  arrange(desc(clone_size_ab))

colnames(clone_table) <- c("Clone_ID (a_cdr3)", "Clone_Size_ab")

clone_size_freq <- clone_table %>%
  count(`Clone_Size_ab`, name = "Freq")

clone_size_freq <- clone_size_freq %>%
  mutate(Scaled_Frequency = Freq / max(Freq))

write.xlsx(clone_size_freq, "Clone_Size_Distribution_Scaled.xlsx", row.names = FALSE)
############################################################ End | Clone Size Table



############################################################ Start | Barplot of Scaled Clone Size Frequency
p_scaled_barplot <- ggplot(clone_size_freq, aes(x = factor(Clone_Size_ab), y = Scaled_Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Normalized Distribution of Clone Sizes (ab)",
       x = "Clone Size (ab)",
       y = "Scaled Frequency") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6)
  )

ggsave("Normalized_Clone_Size_Distribution.png",
       plot = p_scaled_barplot,
       width = 8, height = 6, dpi = 600, bg = "white")

############################################################ End | Barplot of Scaled Clone Size Frequency


############################################################ Start | Barplot of Top Clones
# ساخت جدول نهایی شامل نام کلون و سایز آن
clone_summary <- full_metadata %>%
  filter(!is.na(a_cdr3), a_cdr3 != "") %>%
  count(a_cdr3, name = "Clone_Size") %>%
  arrange(desc(Clone_Size))

colnames(clone_summary) <- c("Clone_ID (a_cdr3)", "Clone_Size")

# ذخیره جدول در فایل اکسل
write.xlsx(clone_summary, "Clone_Name_and_Size_Table.xlsx", row.names = FALSE)


top20_clones <- clone_summary %>% head(20)

# رسم بار پلات
p_top20 <- ggplot(top20_clones, aes(x = reorder(`Clone_ID (a_cdr3)`, -Clone_Size), y = Clone_Size)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Top 20 Clones by Clone Size",
    x = "Clone ID (a_cdr3)",
    y = "Clone Size (Number of Cells)"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
    plot.title = element_text(size = 14, hjust = 0.5),  # عنوان وسط‌چین
    axis.title = element_text(size = 10)
  )

# ذخیره تصویر با کیفیت بالا و بک‌گراند سفید
ggsave("Top20_Clones_Barplot.png", plot = p_top20,
       width = 10, height = 6, dpi = 600, bg = "white")
############################################################ End | Barplot of Top Clones