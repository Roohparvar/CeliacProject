df <- full_metadata[, c("TRBV", "imm_receptor_Esmaeil")]

df <- df[!(is.na(df$TRBV) | df$TRBV == ""), ]

df$Group <- ifelse(df$imm_receptor_Esmaeil %in% c("Aberrant ab", "Aberrant g"),
                   "Aberrant", "Normal")

df$imm_receptor_Esmaeil <- NULL

df <- data.frame(Cell_ID = seq_len(nrow(df)), df)





library(dplyr)
library(tidyr)

count_table <- df %>%
  group_by(TRBV, Group) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Group, values_from = Count, values_fill = 0)


df %>%
  group_by(Group) %>%
  summarise(Count = n())


count_table <- df %>%
  group_by(TRBV, Group) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Group, values_from = Count, values_fill = 0) %>%
  mutate(
    PerAberrant = Aberrant / 2724,
    PerNormal = Normal / 74328
  )



count_table <- count_table %>%
  mutate(
    Difference = abs(PerAberrant - PerNormal)
  )








# ایجاد جدول Fisher برای هر TRBV
fisher_results <- lapply(1:nrow(count_table), function(i) {
  row <- count_table[i, ]
  matrix_vals <- matrix(c(row$Aberrant, row$Normal,
                          sum(count_table$Aberrant) - row$Aberrant,
                          sum(count_table$Normal) - row$Normal),
                        nrow = 2)
  test <- fisher.test(matrix_vals)
  data.frame(
    TRBV = row$TRBV,
    pvalue = test$p.value,
    odds_ratio = test$estimate
  )
})

# ترکیب نتایج
fisher_df <- do.call(rbind, fisher_results)

# اضافه کردن log2 Fold Change (نسبت Aberrant به Normal)
fisher_df$log2FC <- log2((count_table$Aberrant + 1) / (count_table$Normal + 1))

# نمایش نتایج مرتب‌شده
fisher_df <- fisher_df[order(fisher_df$pvalue), ]













library(ggplot2)
library(ggrepel)

# ساخت ستون log10 p-value
fisher_df$negLog10Pval <- -log10(fisher_df$pvalue)
fisher_df$gene <- fisher_df$TRBV

# ✅ اختیاری: محدود کردن مقدار منفی لاگ ۱۰ برای جلوگیری از نقطه‌های خیلی بالا
# اینجا بالاترین مقدار را روی 50 محدود می‌کنیم (تغییرش بده اگه خواستی)
fisher_df$negLog10Pval <- pmin(fisher_df$negLog10Pval, 50)

# ذخیره تصویر با کیفیت بالا
png("volcano_plot_TRBV_clean.png", width = 2000, height = 1600, res = 300)

ggplot(fisher_df, aes(x = log2FC, y = negLog10Pval)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_text_repel(aes(label = gene), size = 2, max.overlaps = 20) +
  xlab("log2 Fold Change (Aberrant vs Normal)") +
  ylab("-log10(p-value)") +
  ggtitle("Volcano Plot of TRBV Usage") +
  theme_minimal(base_size = 14)+
  theme(plot.title = element_text(hjust = 0.5))

dev.off()
