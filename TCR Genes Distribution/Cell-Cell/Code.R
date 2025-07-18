library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)

full_metadata$Group <- ifelse(full_metadata$imm_receptor_Esmaeil == "Aberrant ab", "Aberrant", "Non-Aberrant")

TRBV_table <- full_metadata %>%
  filter(!is.na(TRBV)) %>%
  count(Group, TRBV) %>%
  pivot_wider(names_from = Group, values_from = n, values_fill = 0)

total_counts <- full_metadata %>%
  count(Group) %>%
  pivot_wider(names_from = Group, values_from = n)

total_aberrant <- total_counts$Aberrant
total_non_aberrant <- total_counts$`Non-Aberrant`

results <- TRBV_table %>%
  rowwise() %>%
  mutate(
    other_aberrant = total_aberrant - Aberrant,
    other_non_aberrant = total_non_aberrant - `Non-Aberrant`,
    p_value = fisher.test(matrix(c(Aberrant, `Non-Aberrant`, other_aberrant, other_non_aberrant), nrow = 2))$p.value,
    log2FC = log2((Aberrant + 1) / (`Non-Aberrant` + 1))
  ) %>%
  ungroup() %>%
  mutate(
    neg_log10_p = -log10(p_value),
    TRBV = as.character(TRBV)
  ) %>%
  filter(!is.na(log2FC), !is.na(neg_log10_p))

# سقف محور y برای clip کردن
y_limit <- 20

results <- results %>%
  mutate(
    neg_log10_p_clipped = ifelse(neg_log10_p > y_limit, y_limit, neg_log10_p)
  )

png("Volcano_TRBV_clipped_no_shape_change.png", width = 1200, height = 800)

ggplot(results, aes(x = log2FC, y = neg_log10_p_clipped)) +
  geom_point(color = "black", size = 3) +
  geom_text_repel(aes(label = TRBV), size = 4, max.overlaps = Inf, force = 2) +
  labs(
    title = "Volcano Plot: TRBV Segment Usage (Clipped y, all labels)",
    x = "log2 Fold Change (Aberrant vs Non-Aberrant)",
    y = "-log10(p-value)"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2)), limits = c(0, y_limit + 3)) +
  theme_minimal(base_size = 16)

dev.off()
