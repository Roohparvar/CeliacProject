library(dplyr)
library(tidyr)
library(readr)


cluster_order <- c("Th1 Mem", "Th17", "Th2/Tfh", "Tregs")
full_metadata$cluster <- factor(full_metadata$cluster, levels = cluster_order)

full_metadata <- full_metadata %>%
  filter(!is.na(cluster) & !is.na(TRAV) & TRAV != "" & !is.na(TRBV) & TRBV != "") %>%
  distinct(cdr_Full_ab, .keep_all = TRUE)

# ----------TRAV + TRBV----------
combo_counts <- full_metadata %>%
  count(TRAV, TRBV, name = "repeat_time")

cluster_counts <- full_metadata %>%
  count(TRAV, TRBV, cluster) %>%
  pivot_wider(names_from = cluster, values_from = n, values_fill = 0)

final_counts <- combo_counts %>%
  left_join(cluster_counts, by = c("TRAV", "TRBV")) %>%
  arrange(desc(repeat_time))

# -----------TRAV-----------
trav_total <- full_metadata %>%
  count(TRAV, name = "total")

trav_by_cluster <- full_metadata %>%
  count(TRAV, cluster) %>%
  pivot_wider(names_from = cluster, values_from = n, values_fill = 0)

trav_table <- trav_total %>%
  left_join(trav_by_cluster, by = "TRAV") %>%
  arrange(desc(total))

# -----------TRBV-----------
trbv_total <- full_metadata %>%
  count(TRBV, name = "total")

trbv_by_cluster <- full_metadata %>%
  count(TRBV, cluster) %>%
  pivot_wider(names_from = cluster, values_from = n, values_fill = 0)

trbv_table <- trbv_total %>%
  left_join(trbv_by_cluster, by = "TRBV") %>%
  arrange(desc(total))

# ----------------------
write_csv(final_counts, "TRAV_TRBV_combo_counts.csv")
write_csv(trav_table, "TRAV_counts_by_cluster.csv")
write_csv(trbv_table, "TRBV_counts_by_cluster.csv")
