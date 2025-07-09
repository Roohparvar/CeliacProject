library(DESeq2)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tibble)
library(ggrepel)

# فیلتر داده‌ها: فقط ژن‌های TRBV و دو گروه ACD و Healthy
trbv_data <- full_metadata %>%
  filter(Diagnosis %in% c("ACD", "Healthy")) %>%
  filter(!is.na(TRBV) & TRBV != "") %>%
  select(Patient, Diagnosis, gene = TRBV)

# شمارش فراوانی هر ژن در هر نمونه (Patient)
count_table <- trbv_data %>%
  group_by(Patient, gene, Diagnosis) %>%
  summarise(count = n(), .groups = "drop")

# تبدیل به wide format: سطر = ژن، ستون = نمونه، مقدار = count
count_matrix <- count_table %>%
  select(Patient, gene, count) %>%
  pivot_wider(names_from = Patient, values_from = count, values_fill = 0)

# ساخت ماتریس شمارش برای DESeq2
count_mat <- count_matrix %>%
  column_to_rownames(var = "gene") %>%
  as.matrix()

# اطلاعات شرایط نمونه‌ها (colData)
sample_conditions <- count_table %>%
  select(Patient, Diagnosis) %>%
  distinct() %>%
  arrange(match(Diagnosis, c("Healthy", "ACD")))

# تنظیم rownames برای sample_conditions
sample_conditions <- as.data.frame(sample_conditions)
rownames(sample_conditions) <- sample_conditions$Patient
sample_conditions$Patient <- NULL

# مطابقت ترتیب ستون‌ها و ردیف‌ها
count_mat <- count_mat[, rownames(sample_conditions)]

# ساخت DESeqDataSet
dds <- DESeqDataSetFromMatrix(countData = count_mat,
                              colData = sample_conditions,
                              design = ~ Diagnosis)

# اجرای DESeq
dds <- DESeq(dds)

# استخراج نتایج مقایسه ACD vs Healthy
res <- results(dds, contrast = c("Diagnosis", "ACD", "Healthy"))
res_df <- as.data.frame(res)
res_df$gene <- rownames(res_df)

# تعیین ژن‌هایی که باید هایلایت شوند
green_genes <- c("TRBV7-4", "TRBV7-2", "TRBV5-5", "TRBV2")

# رنگ‌بندی ژن‌ها
res_df$highlight <- case_when(
  res_df$gene == "TRBV28" ~ "TRBV28",
  res_df$gene %in% green_genes ~ res_df$gene,
  TRUE ~ "Other"
)

# رسم volcano plot
p <- ggplot(res_df, aes(x = log2FoldChange, y = -log10(padj), label = gene)) +
  geom_point(aes(color = highlight), alpha = 0.7, size = 2) +
  scale_color_manual(
    values = c(
      "TRBV28" = "red",
      "TRBV7-4" = "green3",
      "TRBV7-2" = "green3",
      "TRBV5-5" = "green3",
      "TRBV2" = "green3",
      "Other" = "grey50"
    ),
    breaks = c("TRBV28", green_genes)  # فقط ژن‌های مهم در legend
  ) +
  geom_text_repel(size = 3, max.overlaps = Inf) +
  labs(
    title = "TRBV Gene Expression: ACD vs Healthy",
    x = "Log2 Fold Change (ACD vs Healthy)",
    y = "-log10 Adjusted p-value (FDR)",
    color = "Highlighted Genes"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

print(p)

# ذخیره نمودار
ggsave("TRBV_Volcano_ACD_vs_Healthy_Highlighted.png", plot = p, width = 8, height = 7, dpi = 300, bg = "white")


