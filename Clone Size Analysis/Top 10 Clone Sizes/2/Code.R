library(dplyr)
library(ggplot2)



#full_metadata <- full_metadata %>%
  #filter(imm_receptor_Esmaeil %in% c("Aberrant ab", "Aberrant g"))



top10_clones <- full_metadata %>%
  group_by(cdr_Full_ab) %>%
  summarise(clone_size = unique(clone_size_ab)[1]) %>%
  arrange(desc(clone_size)) %>%
  slice_head(n = 10)

top10_clones$cdr_Full_ab <- factor(top10_clones$cdr_Full_ab,
                                   levels = top10_clones$cdr_Full_ab)

png("Top 10 Clone Sizes.png", width = 3000, height = 2000, res = 300)

ggplot(top10_clones, aes(x = cdr_Full_ab, y = clone_size)) +
  geom_bar(stat = "identity", fill = "#2c7fb8") +
  labs(title = "Top 10 Clones by Clone Size", x = "Clone", y = "Clone Size") +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
    axis.text.y = element_text(size = 5),
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold")
  )

dev.off()