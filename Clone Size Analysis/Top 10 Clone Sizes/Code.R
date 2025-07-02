library(dplyr)
library(ggplot2)

# استخراج 10 کلون بزرگ
top10_clones <- full_metadata %>%
  group_by(cdr_Full_ab) %>%
  summarise(
    clone_size = unique(clone_size_ab)[1],
    receptor_type = case_when(
      any(imm_receptor_Esmaeil == "Aberrant ab") ~ "Aberrant ab",
      any(imm_receptor_Esmaeil == "Aberrant g")  ~ "Aberrant g",
      TRUE                                        ~ "Other"
    )
  ) %>%
  arrange(desc(clone_size)) %>%
  slice_head(n = 10)

# تنظیم ترتیب عوامل
top10_clones$cdr_Full_ab <- factor(top10_clones$cdr_Full_ab,
                                   levels = top10_clones$cdr_Full_ab)

# تعریف رنگ‌ها
color_map <- c(
  "Aberrant ab" = "#3A78CE",
  "Aberrant g"  = "#47AD45",
  "Other"       = "grey60"
)

# رسم نمودار
png("Top_10_Clone_Sizes_With_Legend.png", width = 3000, height = 2000, res = 300)

ggplot(top10_clones, aes(x = cdr_Full_ab, y = clone_size, fill = receptor_type)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_map, name = "Receptor Type") +
  labs(
    title = "Top 10 Clones by Clone Size",
    x = "Clone",
    y = "Clone Size"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
    axis.text.y = element_text(size = 5),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  )

dev.off()
