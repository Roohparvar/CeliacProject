library(dplyr)
library(tidyr)
library(ggplot2)
library(ggalluvial)



# input -----------------------------------------------------------------------
group_column <- "Diagnosis"
group_values <- c("Healthy", "RCD-I")
selected_clones <- c(
  "CLVFNTGGFKTIF+CASSTRSGGRTPGSCF",
  "CLVFNTGGFKTIF+CASSLQNWGLKHF",
  "CIVFKAAGNKLTF+CASRAFDSTNEKLFF",
  "CAPRGTSGTYKYIF+CASSFGTGGHEQYF",
  "CADLKAAGNKLTF+CASSFNSGGLDATNEKLFF"
)
# input ----------------------------------------------------------------------


df_selected <- full_metadata %>%
  filter(
    !!sym(group_column) %in% group_values,
    cdr_Full_ab %in% selected_clones
  ) %>%
  select(all_of(group_column), cdr_Full_ab) %>%
  group_by_at(vars(all_of(group_column), "cdr_Full_ab")) %>%
  summarise(Clone_Size = n(), .groups = "drop")



df_wide <- df_selected %>%
  pivot_wider(names_from = all_of(group_column), values_from = Clone_Size, values_fill = 0)

df_long <- df_wide %>%
  pivot_longer(cols = all_of(group_values), names_to = group_column, values_to = "Clone_Size") %>%
  filter(Clone_Size > 0)

p_alluvial <- ggplot(df_long,
                     aes_string(x = group_column, stratum = "cdr_Full_ab", alluvium = "cdr_Full_ab",
                                y = "Clone_Size", fill = "cdr_Full_ab")) +
  geom_flow(alpha = 0.6) +
  geom_stratum(alpha = 0.8) +
  theme_minimal() +
  labs(
    title = paste0("Alluvial Plot of Selected αβ Clones in ", paste(group_values, collapse = " and ")),
    x = x_label,
    y = "Clone Size",
    fill = "Clone ID"
  ) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.5)
  )



ggsave(filename, plot = p_alluvial, width = 12, height = 6, dpi = 600, bg = "white")
