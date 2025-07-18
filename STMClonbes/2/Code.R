# Set default values
full_metadata$color <- "#CBCBCB"
full_metadata$shape <- "circle"


# 1
full_metadata$color[full_metadata$imm_receptor_Esmaeil == "Aberrant ab" &
                      full_metadata$TRAV == "TRAV25" &
                      full_metadata$TRAJ == "TRAJ36" &
                      full_metadata$TRBV == "TRBV27" &
                      full_metadata$TRBJ == "TRBJ1-5"] <- "#eb2525"

# 2
full_metadata$color[full_metadata$imm_receptor_Esmaeil == "Aberrant ab" &
                      full_metadata$TRAV == "TRAV29/DV5" &
                      full_metadata$TRAJ == "TRAJ17" &
                      full_metadata$TRBV == "TRBV27" &
                      full_metadata$TRBJ == "TRBJ1-5"] <- "#eb2525"

# 3
full_metadata$color[full_metadata$imm_receptor_Esmaeil == "Aberrant ab" &
                      full_metadata$TRAV == "TRAV1-1" &
                      full_metadata$TRAJ == "TRAJ4" &
                      full_metadata$TRBV == "TRBV29-1" &
                      full_metadata$TRBJ == "TRBJ1-2"] <- "#f8a41d"

# 4
full_metadata$color[full_metadata$imm_receptor_Esmaeil == "Aberrant ab" &
                      full_metadata$TRAV == "TRAV29/DV5" &
                      full_metadata$TRAJ == "TRAJ49" &
                      full_metadata$TRBV == "TRBV7-9" &
                      full_metadata$TRBJ == "TRBJ2-1"] <- "#6bccdf"

# 5
full_metadata$color[full_metadata$imm_receptor_Esmaeil == "Aberrant ab" &
                      full_metadata$TRAV == "TRAV8-1" &
                      full_metadata$TRAJ == "TRAJ27" &
                      full_metadata$TRBV == "TRBV19" &
                      full_metadata$TRBJ == "TRBJ2-7"] <- "#81509f"

# 6
full_metadata$color[full_metadata$imm_receptor_Esmaeil == "Aberrant ab" &
                      full_metadata$TRAV == "TRAV8-1" &
                      full_metadata$TRAJ == "TRAJ22" &
                      full_metadata$TRBV == "TRBV19" &
                      full_metadata$TRBJ == "TRBJ2-5"] <- "#69bc48"

# 7
full_metadata$color[full_metadata$imm_receptor_Esmaeil == "Aberrant ab" &
                      full_metadata$TRAV == "TRAV10" &
                      full_metadata$TRAJ == "TRAJ39" &
                      full_metadata$TRBV == "TRBV6-5" &
                      full_metadata$TRBJ == "TRBJ1-4"] <- "#b45e3d"

# 8
full_metadata$color[full_metadata$imm_receptor_Esmaeil == "Aberrant ab" &
                      full_metadata$TRAV == "TRAV26-1" &
                      full_metadata$TRAJ == "TRAJ17" &
                      full_metadata$TRBV == "TRBV6-5" &
                      full_metadata$TRBJ == "TRBJ1-4"] <- "#b45e3d"

# 9
full_metadata$color[full_metadata$imm_receptor_Esmaeil == "Aberrant g" &
                      full_metadata$TRGV == "TRGV3" &
                      full_metadata$TRGJ == "TRGJ1"] <- "#9aaf9e"
full_metadata$shape[full_metadata$imm_receptor_Esmaeil == "Aberrant g" &
                      full_metadata$TRGV == "TRGV3" &
                      full_metadata$TRGJ == "TRGJ1"] <- "triangle"

# 10
full_metadata$color[full_metadata$imm_receptor_Esmaeil == "Aberrant g" &
                      full_metadata$TRGV == "TRGV8" &
                      full_metadata$TRGJ == "TRGJP2"] <- "#ca563f"
full_metadata$shape[full_metadata$imm_receptor_Esmaeil == "Aberrant g" &
                      full_metadata$TRGV == "TRGV8" &
                      full_metadata$TRGJ == "TRGJP2"] <- "triangle"

# 11
full_metadata$color[full_metadata$imm_receptor_Esmaeil == "Aberrant g" &
                      full_metadata$TRGV == "TRGV3" &
                      full_metadata$TRGJ == "TRGJP2"] <- "#f4d3c2"
full_metadata$shape[full_metadata$imm_receptor_Esmaeil == "Aberrant g" &
                      full_metadata$TRGV == "TRGV3" &
                      full_metadata$TRGJ == "TRGJP2"] <- "triangle"

# 12
full_metadata$color[full_metadata$imm_receptor_Esmaeil == "Aberrant g" &
                      full_metadata$TRGV == "TRGV10" &
                      full_metadata$TRGJ == "TRGJ1"] <- "#e9d41f"
full_metadata$shape[full_metadata$imm_receptor_Esmaeil == "Aberrant g" &
                      full_metadata$TRGV == "TRGV10" &
                      full_metadata$TRGJ == "TRGJ1"] <- "triangle"






library(ggplot2)
library(patchwork)

# 1. تعریف legend_df با ترکیب مواردی که رنگ و شکل یکسان دارند
legend_df <- data.frame(
  label = c(
    "1,2. TRAV25|TRAJ36|TRBV27|TRBJ1-5 + TRAV29/DV5|TRAJ17|TRBV27|TRBJ1-5",
    "3. TRAV1-1|TRAJ4|TRBV29-1|TRBJ1-2",
    "4. TRAV29/DV5|TRAJ49|TRBV7-9|TRBJ2-1",
    "5. TRAV8-1|TRAJ27|TRBV19|TRBJ2-7",
    "6. TRAV8-1|TRAJ22|TRBV19|TRBJ2-5",
    "7,8. TRAV10|TRAJ39|TRBV6-5|TRBJ1-4 + TRAV26-1|TRAJ17|TRBV6-5|TRBJ1-4",
    "9. TRGV3|TRGJ1",
    "10. TRGV8|TRGJP2",
    "11. TRGV3|TRGJP2",
    "12. TRGV10|TRGJ1"
  ),
  color = c(
    "#eb2525",
    "#f8a41d",
    "#6bccdf",
    "#81509f",
    "#69bc48",
    "#b45e3d",
    "#9aaf9e",
    "#ca563f",
    "#f4d3c2",
    "#e9d41f"
  ),
  shape = c(
    "circle", "circle", "circle", "circle", "circle",
    "circle", "triangle", "triangle", "triangle", "triangle"
  ),
  stringsAsFactors = FALSE
)

# 2. ستون legend_label را به full_metadata اضافه کن و پرش کن
full_metadata$legend_label <- NA_character_
for (i in seq_len(nrow(legend_df))) {
  row <- legend_df[i, ]
  mask <- full_metadata$color == row$color & full_metadata$shape == row$shape
  full_metadata$legend_label[mask] <- row$label
}

# 3. مرتب‌سازی legend_label برای لگند: اول دایره‌ها، بعد مثلث‌ها
circle_labels <- legend_df$label[legend_df$shape == "circle"]
triangle_labels <- legend_df$label[legend_df$shape == "triangle"]
full_metadata$legend_label <- factor(full_metadata$legend_label,
                                     levels = c(circle_labels, triangle_labels))

# 4. رسم UMAP اصلی با همه نقاط
main_plot <- ggplot() +
  geom_point(data = full_metadata[full_metadata$color == "#CBCBCB", ],
             aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2),
             color = "#CBCBCB", size = 0.5, alpha = 0.5, shape = 16) +
  geom_point(data = full_metadata[full_metadata$color != "#CBCBCB", ],
             aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2,
                 color = legend_label, shape = legend_label),
             size = 2, alpha = 0.9) +
  scale_color_manual(values = setNames(legend_df$color, legend_df$label)) +
  scale_shape_manual(values = setNames(ifelse(legend_df$shape == "circle", 16, 17),
                                       legend_df$label)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 18)  # وسط‌چین کردن عنوان
  ) +
  labs(title = "Distribution of Aberrant clones reported in STM")

# 5. لگند دایره‌ها (بدون نقاط واقعی، فقط برای نمایش لگند)
p_circles <- ggplot() +
  geom_point(aes(x = 1, y = 1,
                 color = legend_df$label[legend_df$shape == "circle"],
                 shape = legend_df$label[legend_df$shape == "circle"]),
             size = 6) +  # اندازه دایره‌ها در لگند
  scale_color_manual(values = setNames(legend_df$color[legend_df$shape == "circle"],
                                       legend_df$label[legend_df$shape == "circle"])) +
  scale_shape_manual(values = setNames(rep(16, sum(legend_df$shape == "circle")),
                                       legend_df$label[legend_df$shape == "circle"])) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(ncol = 1, override.aes = list(size = 6)),
         shape = guide_legend(ncol = 1, override.aes = list(size = 6))) +
  labs(color = "", shape = "")

# 6. لگند مثلث‌ها (بدون نقاط واقعی، فقط برای نمایش لگند)
p_triangles <- ggplot() +
  geom_point(aes(x = 1, y = 1,
                 color = legend_df$label[legend_df$shape == "triangle"],
                 shape = legend_df$label[legend_df$shape == "triangle"]),
             size = 6) +  # اندازه مثلث‌ها در لگند
  scale_color_manual(values = setNames(legend_df$color[legend_df$shape == "triangle"],
                                       legend_df$label[legend_df$shape == "triangle"])) +
  scale_shape_manual(values = setNames(rep(17, sum(legend_df$shape == "triangle")),
                                       legend_df$label[legend_df$shape == "triangle"])) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  guides(color = guide_legend(ncol = 1, override.aes = list(size = 6)),
         shape = guide_legend(ncol = 1, override.aes = list(size = 6))) +
  labs(color = "", shape = "")

# 7. کنار هم گذاشتن لگندها در 2 ستون: دایره‌ها و مثلث‌ها
legend_combined <- p_circles + p_triangles + patchwork::plot_layout(ncol = 2, widths = c(1,1))

# 8. ترکیب UMAP و لگندها، لگند زیر نمودار، لگند 1/4 ارتفاع کل فضا
final_plot <- main_plot / legend_combined + patchwork::plot_layout(heights = c(4, 1))

# 9. ذخیره عکس
ggsave("umap_tcr_signatures_ordered_legend.png", plot = final_plot,
       width = 14, height = 12, dpi = 300, bg = "white")























full_metadata <- subset(full_metadata,
                        (imm_receptor_Esmaeil == "Aberrant ab" & TRAV == "TRAV25" & TRAJ == "TRAJ36" & TRBV == "TRBV27" & TRBJ == "TRBJ1-5") |
                          (imm_receptor_Esmaeil == "Aberrant ab" & TRAV == "TRAV29/DV5" & TRAJ == "TRAJ17" & TRBV == "TRBV27" & TRBJ == "TRBJ1-5") |
                          (imm_receptor_Esmaeil == "Aberrant ab" & TRAV == "TRAV1-1" & TRAJ == "TRAJ4" & TRBV == "TRBV29-1" & TRBJ == "TRBJ1-2") |
                          (imm_receptor_Esmaeil == "Aberrant ab" & TRAV == "TRAV29/DV5" & TRAJ == "TRAJ49" & TRBV == "TRBV7-9" & TRBJ == "TRBJ2-1") |
                          (imm_receptor_Esmaeil == "Aberrant ab" & TRAV == "TRAV8-1" & TRAJ == "TRAJ27" & TRBV == "TRBV19" & TRBJ == "TRBJ2-7") |
                          (imm_receptor_Esmaeil == "Aberrant ab" & TRAV == "TRAV8-1" & TRAJ == "TRAJ22" & TRBV == "TRBV19" & TRBJ == "TRBJ2-5") |
                          (imm_receptor_Esmaeil == "Aberrant ab" & TRAV == "TRAV10" & TRAJ == "TRAJ39" & TRBV == "TRBV6-5" & TRBJ == "TRBJ1-4") |
                          (imm_receptor_Esmaeil == "Aberrant ab" & TRAV == "TRAV26-1" & TRAJ == "TRAJ17" & TRBV == "TRBV6-5" & TRBJ == "TRBJ1-4") |
                          (imm_receptor_Esmaeil == "Aberrant g" & TRGV == "TRGV3" & TRGJ == "TRGJ1") |
                          (imm_receptor_Esmaeil == "Aberrant g" & TRGV == "TRGV8" & TRGJ == "TRGJP2") |
                          (imm_receptor_Esmaeil == "Aberrant g" & TRGV == "TRGV3" & TRGJ == "TRGJP2") |
                          (imm_receptor_Esmaeil == "Aberrant g" & TRGV == "TRGV10" & TRGJ == "TRGJ1")
)


conditions <- list(
  full_metadata$imm_receptor_Esmaeil == "Aberrant ab" & full_metadata$TRAV == "TRAV25" & full_metadata$TRAJ == "TRAJ36" & full_metadata$TRBV == "TRBV27" & full_metadata$TRBJ == "TRBJ1-5",
  full_metadata$imm_receptor_Esmaeil == "Aberrant ab" & full_metadata$TRAV == "TRAV29/DV5" & full_metadata$TRAJ == "TRAJ17" & full_metadata$TRBV == "TRBV27" & full_metadata$TRBJ == "TRBJ1-5",
  full_metadata$imm_receptor_Esmaeil == "Aberrant ab" & full_metadata$TRAV == "TRAV1-1" & full_metadata$TRAJ == "TRAJ4" & full_metadata$TRBV == "TRBV29-1" & full_metadata$TRBJ == "TRBJ1-2",
  full_metadata$imm_receptor_Esmaeil == "Aberrant ab" & full_metadata$TRAV == "TRAV29/DV5" & full_metadata$TRAJ == "TRAJ49" & full_metadata$TRBV == "TRBV7-9" & full_metadata$TRBJ == "TRBJ2-1",
  full_metadata$imm_receptor_Esmaeil == "Aberrant ab" & full_metadata$TRAV == "TRAV8-1" & full_metadata$TRAJ == "TRAJ27" & full_metadata$TRBV == "TRBV19" & full_metadata$TRBJ == "TRBJ2-7",
  full_metadata$imm_receptor_Esmaeil == "Aberrant ab" & full_metadata$TRAV == "TRAV8-1" & full_metadata$TRAJ == "TRAJ22" & full_metadata$TRBV == "TRBV19" & full_metadata$TRBJ == "TRBJ2-5",
  full_metadata$imm_receptor_Esmaeil == "Aberrant ab" & full_metadata$TRAV == "TRAV10" & full_metadata$TRAJ == "TRAJ39" & full_metadata$TRBV == "TRBV6-5" & full_metadata$TRBJ == "TRBJ1-4",
  full_metadata$imm_receptor_Esmaeil == "Aberrant ab" & full_metadata$TRAV == "TRAV26-1" & full_metadata$TRAJ == "TRAJ17" & full_metadata$TRBV == "TRBV6-5" & full_metadata$TRBJ == "TRBJ1-4",
  full_metadata$imm_receptor_Esmaeil == "Aberrant g" & full_metadata$TRGV == "TRGV3" & full_metadata$TRGJ == "TRGJ1",
  full_metadata$imm_receptor_Esmaeil == "Aberrant g" & full_metadata$TRGV == "TRGV8" & full_metadata$TRGJ == "TRGJP2",
  full_metadata$imm_receptor_Esmaeil == "Aberrant g" & full_metadata$TRGV == "TRGV3" & full_metadata$TRGJ == "TRGJP2",
  full_metadata$imm_receptor_Esmaeil == "Aberrant g" & full_metadata$TRGV == "TRGV10" & full_metadata$TRGJ == "TRGJ1"
)

for (i in seq_along(conditions)) {
  count <- sum(conditions[[i]], na.rm = TRUE)
  cat("condition", i, ":", count, "Cells\n")
}

