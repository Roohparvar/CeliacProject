setwd("C:/Esmaeil/CeliacProject/CeliacProject/6_TCR genotype distribution/αβ/CD4/TRBV/1_Healthy and ACD")

clean_data <- full_metadata[
  !is.na(full_metadata$TRBV) &
    !is.na(full_metadata$Patient) &
    full_metadata$cluster %in% c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs") &
    full_metadata$Diagnosis %in% c("Healthy", "ACD"),
]


count_matrix <- as.data.frame.matrix(table(clean_data$TRBV, clean_data$Patient))

percent_matrix <- sweep(count_matrix, 2, colSums(count_matrix), FUN = "/") * 100
count_matrix = percent_matrix


healthy_samples <- colnames(count_matrix)[colnames(count_matrix) %in% unique(clean_data$Patient[clean_data$Diagnosis == "Healthy"])]
acd_samples <- colnames(count_matrix)[colnames(count_matrix) %in% unique(clean_data$Patient[clean_data$Diagnosis == "ACD"])]


mean_healthy <- rowMeans(count_matrix[, healthy_samples, drop = FALSE])
mean_acd <- rowMeans(count_matrix[, acd_samples, drop = FALSE])


log2fc <- log2( (mean_acd + 1) / (mean_healthy + 1) )
count_matrix$log2FC <- log2fc



pvals <- apply(count_matrix[, c(healthy_samples, acd_samples), drop = FALSE], 1, function(x) {
  healthy_values <- as.numeric(x[healthy_samples])
  acd_values <- as.numeric(x[acd_samples])
  
  
  if(length(healthy_values) < 2 || length(acd_values) < 2) {
    return(NA)
  }
  
  test <- wilcox.test(healthy_values, acd_values)
  return(test$p.value)
})

count_matrix$pvalue <- pvals
count_matrix <- count_matrix[!is.na(count_matrix$pvalue), ]
count_matrix$FDR <- p.adjust(count_matrix$pvalue, method = "BH")
count_matrix$negLog10P <- -log10(count_matrix$pvalue)



top_genes <- head(count_matrix[order(count_matrix$pvalue), ], 1)
top_names <- rownames(top_genes)
colors <- c("#F58231", "blue", "green", "purple", "orange")

png("volcano_plot.png", width = 1300, height = 1600, res = 300)

plot(
  count_matrix$log2FC,
  count_matrix$negLog10P,
  xlab = "% difference TRBV gene use(ACD versus Healthy)",
  ylab = "-log10(p-value)",
  pch = 20,
  col = "black",
  xlim = range(count_matrix$log2FC) + c(0, 4)
)

abline(h = 1.3, lty = 2, col = "gray40")   
abline(v = 0, lty = 2, col = "gray40")

title(main = "", line = 2, adj = 0.5)

for (i in seq_along(top_names)) {
  gene <- top_names[i]
  points(
    count_matrix[gene, "log2FC"],
    count_matrix[gene, "negLog10P"],
    pch = 20,
    col = colors[i],
    cex = 1.5
  )
}

par(xpd = TRUE)

legend(
  x = max(count_matrix$log2FC) + 1.5, y = max(count_matrix$negLog10P),
  legend = top_names,
  col = colors,
  pch = 20,
  cex = 1,
  title = "Top Gene"
)

dev.off()


# Save as PNG
png("volcano_plot.png", width = 1300, height = 1600, res = 300)

plot(
  count_matrix$log2FC,
  count_matrix$negLog10P,
  xlab = "% difference TRBV gene use(ACD versus Healthy)",
  ylab = "-log10(p-value)",
  pch = 20,
  col = "black",
  xlim = range(count_matrix$log2FC) + c(0, 4)
)

abline(h = 1.3, lty = 2, col = "gray40")
abline(v = 0, lty = 2, col = "gray40")

title(main = "", line = 2, adj = 0.5)

for (i in seq_along(top_names)) {
  gene <- top_names[i]
  points(
    count_matrix[gene, "log2FC"],
    count_matrix[gene, "negLog10P"],
    pch = 20,
    col = colors[i],
    cex = 1.5
  )
}

par(xpd = TRUE)
legend(
  x = max(count_matrix$log2FC) + 1.5, y = max(count_matrix$negLog10P),
  legend = top_names,
  col = colors,
  pch = 20,
  cex = 1,
  title = "Top Gene"
)

dev.off()


# Save as PDF
pdf("volcano_plot.pdf", width = 6, height = 7)

plot(
  count_matrix$log2FC,
  count_matrix$negLog10P,
  xlab = "% difference TRBV gene use(ACD versus Healthy)",
  ylab = "-log10(p-value)",
  pch = 20,
  col = "black",
  xlim = range(count_matrix$log2FC) + c(0, 4)
)

abline(h = 1.3, lty = 2, col = "gray40")
abline(v = 0, lty = 2, col = "gray40")

title(main = "", line = 2, adj = 0.5)

for (i in seq_along(top_names)) {
  gene <- top_names[i]
  points(
    count_matrix[gene, "log2FC"],
    count_matrix[gene, "negLog10P"],
    pch = 20,
    col = colors[i],
    cex = 1.5
  )
}

par(xpd = TRUE)
legend(
  x = max(count_matrix$log2FC) + 1.5, y = max(count_matrix$negLog10P),
  legend = top_names,
  col = colors,
  pch = 20,
  cex = 1,
  title = "Top Gene"
)

dev.off()