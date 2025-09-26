clean_data <- full_metadata[
  !is.na(full_metadata$TRGV) &
    !is.na(full_metadata$Patient) &
    full_metadata$cluster %in% c("IEL GZMK+", "Trm IEL", "Prolif. IEL", "Cyt. IEL", "IEL CCL4+", "nIEL") &
    full_metadata$Diagnosis %in% c("Healthy", "RCD-I"),
]


count_matrix <- as.data.frame.matrix(table(clean_data$TRGV, clean_data$Patient))

percent_matrix <- sweep(count_matrix, 2, colSums(count_matrix), FUN = "/") * 100
count_matrix = percent_matrix


Healthy_samples <- colnames(count_matrix)[colnames(count_matrix) %in% unique(clean_data$Patient[clean_data$Diagnosis == "Healthy"])]
RCD1_samples <- colnames(count_matrix)[colnames(count_matrix) %in% unique(clean_data$Patient[clean_data$Diagnosis == "RCD-I"])]


mean_healthy <- rowMeans(count_matrix[, Healthy_samples, drop = FALSE])
mean_RCD1 <- rowMeans(count_matrix[, RCD1_samples, drop = FALSE])


log2fc <- log2( (mean_RCD1 + 1) / (mean_healthy + 1) )
count_matrix$log2FC <- log2fc



pvals <- apply(count_matrix[, c(Healthy_samples, RCD1_samples), drop = FALSE], 1, function(x) {
  healthy_values <- as.numeric(x[Healthy_samples])
  RCD1_values <- as.numeric(x[RCD1_samples])
  
  
  if(length(healthy_values) < 2 || length(RCD1_values) < 2) {
    return(NA)
  }
  
  test <- t.test(healthy_values, RCD1_values)
  return(test$p.value)
})

count_matrix$pvalue <- pvals
count_matrix <- count_matrix[!is.na(count_matrix$pvalue), ]
count_matrix$FDR <- p.adjust(count_matrix$pvalue, method = "BH")
count_matrix$negLog10P <- -log10(count_matrix$pvalue)



top_genes <- head(count_matrix[order(count_matrix$pvalue), ], 2)
top_names <- rownames(top_genes)
colors <- c("red", "blue", "green", "purple", "orange")

png("volcano_plot_pvalue_top5_legend_outside.png", width = 1300, height = 1600, res = 300)

plot(
  count_matrix$log2FC,
  count_matrix$negLog10P,
  xlab = "% difference TRGV gene use(RCD1 versus Healthy)",
  ylab = "-log10(p-value)",
  pch = 20,
  col = "black",
  xlim = range(count_matrix$log2FC) + c(0, 4)
)

abline(h = 1.3, lty = 2, col = "gray40")   
abline(v = 0, lty = 2, col = "gray40")

title(main = "Volcano plot of TRGV segment usage", line = 2, adj = 0.5)

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
  title = "Top Genes"
)

dev.off()
