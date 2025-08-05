# 1. فیلتر داده‌ها فقط برای TRBVهای غیر NA و بیماران Healthy و ACD
clean_data <- full_metadata[
  !is.na(full_metadata$TRBV) &
    !is.na(full_metadata$Patient) &
    full_metadata$Diagnosis %in% c("ACD", "Healthy"),
]

# 2. ساخت ماتریس شمارش ژن‌ها در بیماران
count_matrix <- as.data.frame.matrix(table(clean_data$TRBV, clean_data$Patient))

# 3. گرفتن اسامی بیماران هر گروه
healthy_samples <- colnames(count_matrix)[colnames(count_matrix) %in% unique(clean_data$Patient[clean_data$Diagnosis == "Healthy"])]
acd_samples <- colnames(count_matrix)[colnames(count_matrix) %in% unique(clean_data$Patient[clean_data$Diagnosis == "ACD"])]

# 4. محاسبه میانگین شمارش ژن‌ها در هر گروه
mean_healthy <- rowMeans(count_matrix[, healthy_samples, drop = FALSE])
mean_acd <- rowMeans(count_matrix[, acd_samples, drop = FALSE])

# 5. محاسبه log2 Fold Change (اضافه 1 برای جلوگیری از تقسیم بر صفر)
log2fc <- log2( (mean_acd + 1) / (mean_healthy + 1) )
count_matrix$log2FC <- log2fc




# 6. محاسبه p-value با استفاده از t-test برای هر ژن
pvals <- apply(count_matrix[, c(healthy_samples, acd_samples), drop = FALSE], 1, function(x) {
  healthy_values <- as.numeric(x[healthy_samples])
  acd_values <- as.numeric(x[acd_samples])
  
  # اگر تعداد نمونه‌ها کمتر از 2 باشد p-value NA می‌شود
  if(length(healthy_values) < 2 || length(acd_values) < 2) {
    return(NA)
  }
  
  test <- t.test(healthy_values, acd_values)
  return(test$p.value)
})

count_matrix$pvalue <- pvals

# 7. حذف ردیف‌هایی که p-value ندارند (NA)
count_matrix <- count_matrix[!is.na(count_matrix$pvalue), ]

# 8. محاسبه FDR با روش Benjamini-Hochberg
count_matrix$FDR <- p.adjust(count_matrix$pvalue, method = "BH")









# اضافه کردن ستون -log10(pvalue)
count_matrix$negLog10P <- -log10(count_matrix$pvalue)

# انتخاب ۵ ژن با کوچک‌ترین p-value
top5_genes <- head(count_matrix[order(count_matrix$pvalue), ], 5)
top5_names <- rownames(top5_genes)

# رنگ‌ها برای ۵ ژن
colors <- c("red", "blue", "green", "purple", "orange")

# شروع ذخیره تصویر با کیفیت بالا
png("volcano_plot_pvalue_top5_legend_outside.png", width = 2200, height = 1600, res = 300)

# رسم تمام ژن‌ها به رنگ مشکی
plot(
  count_matrix$log2FC,
  count_matrix$negLog10P,
  xlab = "log2 Fold Change (ACD versus Healthy)",
  ylab = "-log10(p-value)",
  main = "Volcano Plot with Top 5 Genes Highlighted",
  pch = 20,
  col = "black",
  xlim = range(count_matrix$log2FC) + c(0, 3) # فضای بیشتر سمت راست برای لجند
)

# اضافه کردن ۵ ژن اول با رنگ‌های مشخص
for (i in seq_along(top5_names)) {
  gene <- top5_names[i]
  points(
    count_matrix[gene, "log2FC"],
    count_matrix[gene, "negLog10P"],
    pch = 20,
    col = colors[i],
    cex = 1.5
  )
}

# اضافه کردن لجند بیرون نمودار
par(xpd = TRUE)  # اجازه رسم خارج از ناحیه نمودار

legend(
  x = max(count_matrix$log2FC) + 1.5, y = max(count_matrix$negLog10P),
  legend = top5_names,
  col = colors,
  pch = 20,
  cex = 1,
  title = "Top 5 Genes"
)

dev.off()

