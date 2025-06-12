library(dplyr)
library(writexl)
library(ggplot2)

# ------------------------------------------------------------------------------ G_CDR3 Combinations Summary
combo_counts <- tibble(
  Description = c(
    "g_cdr3 present, a_cdr3, b_cdr3, d_cdr3 absent",
    "g_cdr3 and a_cdr3 present, b_cdr3 and d_cdr3 absent",
    "g_cdr3 and b_cdr3 present, a_cdr3 and d_cdr3 absent",
    "g_cdr3, a_cdr3 and b_cdr3 present, d_cdr3 absent"
  ),
  `Cell Count` = c(
    full_metadata %>%
      filter(!is.na(g_cdr3) & is.na(a_cdr3) & is.na(b_cdr3) & is.na(d_cdr3)) %>%
      nrow(),
    
    full_metadata %>%
      filter(!is.na(g_cdr3) & !is.na(a_cdr3) & is.na(b_cdr3) & is.na(d_cdr3)) %>%
      nrow(),
    
    full_metadata %>%
      filter(!is.na(g_cdr3) & is.na(a_cdr3) & !is.na(b_cdr3) & is.na(d_cdr3)) %>%
      nrow(),
    
    full_metadata %>%
      filter(!is.na(g_cdr3) & !is.na(a_cdr3) & !is.na(b_cdr3) & is.na(d_cdr3)) %>%
      nrow()
  )
)

write_xlsx(combo_counts, "G_CDR3_Combinations_Summary.xlsx")



# ------------------------------------------------------------------------------ Umap | G_CDR3 Combinations Summary
umap_matrix <- full_metadata %>%
  select(
    scVI_with_hvg_UMAP_1, scVI_with_hvg_UMAP_2,
    a_cdr3, b_cdr3, g_cdr3, d_cdr3
  )

umap_matrix$`ColourG!ABD`    <- "gray"
umap_matrix$`ColourGA!BD`    <- "gray"
umap_matrix$`ColourGB!AD`    <- "gray"
umap_matrix$`ColourGAB!D`    <- "gray"


umap_matrix$`ColourG!ABD`[!is.na(umap_matrix$g_cdr3) &
                            is.na(umap_matrix$a_cdr3) &
                            is.na(umap_matrix$b_cdr3) &
                            is.na(umap_matrix$d_cdr3)] <- "red"

umap_matrix$`ColourGA!BD`[!is.na(umap_matrix$g_cdr3) &
                            !is.na(umap_matrix$a_cdr3) &
                            is.na(umap_matrix$b_cdr3) &
                            is.na(umap_matrix$d_cdr3)] <- "blue"

umap_matrix$`ColourGB!AD`[!is.na(umap_matrix$g_cdr3) &
                            is.na(umap_matrix$a_cdr3) &
                            !is.na(umap_matrix$b_cdr3) &
                            is.na(umap_matrix$d_cdr3)] <- "green"

umap_matrix$`ColourGAB!D`[!is.na(umap_matrix$g_cdr3) &
                            !is.na(umap_matrix$a_cdr3) &
                            !is.na(umap_matrix$b_cdr3) &
                            is.na(umap_matrix$d_cdr3)] <- "yellow"


png("UMAP_ColourG_only.png", width = 2000, height = 1600, res = 300)
ggplot(umap_matrix, aes(
  x = scVI_with_hvg_UMAP_1,
  y = scVI_with_hvg_UMAP_2
)) +
  geom_point(color = umap_matrix$`ColourG!ABD`, size = 0.2, alpha = 0.8) +
  labs(title = "Colored cells have g_cdr3 present, but a_cdr3, b_cdr3, and d_cdr3 absent.", x = "UMAP 1", y = "UMAP 2") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )
dev.off()

# UMAP - G and A
png("UMAP_ColourG_and_A.png", width = 2000, height = 1600, res = 300)
ggplot(umap_matrix, aes(
  x = scVI_with_hvg_UMAP_1,
  y = scVI_with_hvg_UMAP_2
)) +
  geom_point(color = umap_matrix$`ColourGA!BD`, size = 0.2, alpha = 0.8) +
  labs(title = "Colored cells have g_cdr3 and a_cdr3 present, but b_cdr3 and d_cdr3 absent", x = "UMAP 1", y = "UMAP 2") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )
dev.off()

# UMAP - G and B
png("UMAP_ColourG_and_B.png", width = 2000, height = 1600, res = 300)
ggplot(umap_matrix, aes(
  x = scVI_with_hvg_UMAP_1,
  y = scVI_with_hvg_UMAP_2
)) +
  geom_point(color = umap_matrix$`ColourGB!AD`, size = 0.2, alpha = 0.8) +
  labs(title = "Colored cells have g_cdr3 and b_cdr3 present, but a_cdr3 and d_cdr3 absent", x = "UMAP 1", y = "UMAP 2") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )
dev.off()

# UMAP - G and A and B
png("UMAP_ColourG_A_B.png", width = 2000, height = 1600, res = 300)
ggplot(umap_matrix, aes(
  x = scVI_with_hvg_UMAP_1,
  y = scVI_with_hvg_UMAP_2
)) +
  geom_point(color = umap_matrix$`ColourGAB!D`, size = 0.2, alpha = 0.8) +
  labs(title = "Colored cells have g_cdr3, a_cdr3, and b_cdr3 present, but d_cdr3 absent", x = "UMAP 1", y = "UMAP 2") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8)
  )
dev.off()
