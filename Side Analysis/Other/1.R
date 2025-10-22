library(dplyr)
library(ggplot2)

# لیست ژن‌های مورد نظر (می‌تونی بقیه ژن‌ها رو هم اضافه کنی)
genes_of_interest <- c(
  "TRAV29/DV5", "TRAV1-1", "TRAV25", "TRAV13-1", "TRAV20", "TRAV26-1", "TRAV10", "TRAV8-1",
  "TRAJ17", "TRAJ49", "TRAJ4", "TRAJ36", "TRAJ39", "TRAJ27", "TRAJ22",
  "TRBV27", "TRBV7-9", "TRBV29-1", "TRBV6-5", "TRBV19",
  "TRBJ1-5", "TRBJ2-1", "TRBJ1-2", "TRBJ1-4", "TRBJ2-7", "TRBJ2-5",
  "TRGV10", "TRGV3", "TRGV2", "TRGV8", "TRGV5", "TRGV5P", "TRGV4",
  "TRGJ1", "TRGJP2",
  "TRDV1", "TRDV2", "TRDV3",
  "TRDJ1", "TRDJ2", "TRDJ3", "TRAJ37", "TRAJ48", "TRAJ30", "TRAJ18", "TRAJ49", "TRAJ20"
)

# ستون‌هایی که باید بررسی شوند
gene_columns <- c("TRAV", "TRAJ", "TRBJ", "TRBV", "TRGV", "TRGJ", "TRDV", "TRDJ")

# تعریف گروه‌ها
full_metadata$UsageGroup <- case_when(
  full_metadata$imm_receptor_Esmaeil == "Aberrant ab" ~ "Aberrant ab",
  full_metadata$imm_receptor_Esmaeil == "Aberrant g" ~ "Aberrant g",
  TRUE ~ "Non-Aberrant"
)

# شمارش استفاده از ژن‌ها در هر گروه
all_counts <- list()
for (gene in genes_of_interest) {
  for (grp in unique(full_metadata$UsageGroup)) {
    subset_df <- full_metadata[full_metadata$UsageGroup == grp, ]
    count <- 0
    for (col in gene_columns) {
      count <- count + sum(subset_df[[col]] == gene, na.rm = TRUE)
    }
    all_counts[[length(all_counts) + 1]] <- data.frame(Gene = gene, Group = grp, Count = count)
  }
}

# ساخت دیتافریم نهایی
df_counts <- bind_rows(all_counts)
df_counts$Gene <- factor(df_counts$Gene, levels = unique(genes_of_interest))
# رسم نمودار میله‌ای
p <- ggplot(df_counts, aes(x = Gene, y = Count, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Aberrant ab" = "#d62728", "Aberrant g" = "#2ca02c", "Non-Aberrant" = "#1f77b4")) +
  labs(title = "Gene Usage in T Cell Clones",
       x = "Gene Segment", y = "Usage Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))

# ذخیره به صورت تصویر PNG با کیفیت بالا و بک‌گراند سفید
ggsave("gene_usage_barplot.png", plot = p, width = 16, height = 6, dpi = 300, bg = "white")
