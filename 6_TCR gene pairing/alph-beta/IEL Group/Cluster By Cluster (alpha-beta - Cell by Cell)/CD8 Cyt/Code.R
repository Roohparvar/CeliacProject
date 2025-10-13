setwd("C:/Esmaeil/CeliacProject/CeliacProject/Pairing/alph-beta/IEL Group/Cluster By Cluster (alpha-beta - Cell by Cell)/CD8 Cyt")


library(dplyr)
library(ggplot2)
library(ggalluvial)
library(writexl)

full_metadata = full_metadata[full_metadata$cluster == "CD8 Cyt.", ]

full_metadata <- full_metadata %>% filter(!is.na(TRAV) & TRAV != "")
full_metadata <- full_metadata %>% filter(!is.na(TRBV) & TRBV != "")



target_travs <- c("TRAV12-1", "TRAV12-2", "TRAV30", "TRAV8-4", "TRAV8-6")
full_metadata <- full_metadata %>% mutate(TRAV = ifelse(TRAV %in% target_travs, TRAV, "Other TRAVs"))
full_metadata$TRAV <- factor(full_metadata$TRAV,
                             levels = c("TRAV12-1", "TRAV12-2", "TRAV30", "TRAV8-4", "TRAV8-6", "Other TRAVs"))



trav_summary <- full_metadata %>%
  group_by(Diagnosis, TRAV) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Diagnosis) %>%
  mutate(total_cells = sum(count),
         percent = round((count / total_cells) * 100, 2)) %>%
  arrange(Diagnosis, TRAV)



all_TRAVs <- c("TRAV12-1", "TRAV12-2", "TRAV30", "TRAV8-4", "TRAV8-6", "Other TRAVs")

trav_summary <- trav_summary %>%
  group_by(Diagnosis) %>%
  # create all combinations of Diagnosis x TRAV
  complete(TRAV = all_TRAVs, fill = list(count = 0, percent = 0)) %>%
  # total_cells should be the same for each Diagnosis
  mutate(total_cells = max(total_cells, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(Diagnosis, factor(TRAV, levels = all_TRAVs))



write_xlsx(trav_summary, "TRAV_summary.xlsx")