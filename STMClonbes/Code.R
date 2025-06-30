safe_equal <- function(a, b) {
  !is.na(a) && a == b
}

is_blank <- function(x) {
  is.na(x) || x == ""
}


full_metadata$color <- NA
full_metadata$shape <- NA
match_counts <- numeric(12)


for (i in 1:nrow(full_metadata)) {
  cat(i,"\n")
  row <- full_metadata[i, ]
  
  # 1 Ok 238
  if (safe_equal(row$TRAV, "TRAV29/DV5") && safe_equal(row$TRAJ, "TRAJ36") &&
      safe_equal(row$TRBV, "TRBV27") && safe_equal(row$TRBJ, "TRBJ1-5") &&
      is_blank(row$TRGV) && is_blank(row$TRGJ) && 
      is_blank(row$TRDV) && is_blank(row$TRDJ)) {
    full_metadata[i, c("color", "shape")] <- c("#eb2525", "circle")
    match_counts[1] <- match_counts[1] + 1
    next
  }
  
  # 2 Ok 497
  if (safe_equal(row$TRAV, "TRAV29/DV5") && safe_equal(row$TRAJ, "TRAJ17") &&
      safe_equal(row$TRBV, "TRBV27") && safe_equal(row$TRBJ, "TRBJ1-5") &&
      is_blank(row$TRGV) && is_blank(row$TRGJ)&& 
      is_blank(row$TRDV) && is_blank(row$TRDJ)) {
    full_metadata[i, c("color", "shape")] <- c("#eb2525", "circle")
    match_counts[2] <- match_counts[2] + 1
    next
  }
  
  # 3 Ok 41
  if (safe_equal(row$TRAV, "TRAV1-1") && safe_equal(row$TRAJ, "TRAJ4") &&
      safe_equal(row$TRBV, "TRBV29-1") && safe_equal(row$TRBJ, "TRBJ1-2") &&
      is_blank(row$TRGV) && is_blank(row$TRGJ)&& 
      is_blank(row$TRDV) && is_blank(row$TRDJ)) {
    full_metadata[i, c("color", "shape")] <- c("#f8a41d", "circle")
    match_counts[3] <- match_counts[3] + 1
    next
  }
  
  # 4 Ok 27
  if (safe_equal(row$TRAV, "TRAV29/DV5") && safe_equal(row$TRAJ, "TRAJ49") &&
      safe_equal(row$TRBV, "TRBV7-9") && safe_equal(row$TRBJ, "TRBJ2-1") &&
      is_blank(row$TRGV) && is_blank(row$TRGJ)&& 
      is_blank(row$TRDV) && is_blank(row$TRDJ)) {
    full_metadata[i, c("color", "shape")] <- c("#6bccdf", "circle")
    match_counts[4] <- match_counts[4] + 1
    next
  }
  
  # 5
  if (safe_equal(row$TRAV, "TRAV8") && safe_equal(row$TRAJ, "TRAJ27") &&
      safe_equal(row$TRBV, "TRBV19") && safe_equal(row$TRBJ, "TRBJ2-8") &&
      is_blank(row$TRGV) && is_blank(row$TRGJ)&& 
      is_blank(row$TRDV) && is_blank(row$TRDJ)) {
    full_metadata[i, c("color", "shape")] <- c("#81509f", "circle")
    match_counts[5] <- match_counts[5] + 1
    next
  }
  
  # 6
  if (safe_equal(row$TRAV, "TRAV8") && safe_equal(row$TRAJ, "TRAJ22") &&
      safe_equal(row$TRBV, "TRBV19") && safe_equal(row$TRBJ, "TRBJ2-5") &&
      is_blank(row$TRGV) && is_blank(row$TRGJ)&& 
      is_blank(row$TRDV) && is_blank(row$TRDJ)) {
    full_metadata[i, c("color", "shape")] <- c("#69bc48", "circle")
    match_counts[6] <- match_counts[6] + 1
    next
  }
  
  #7
  if (safe_equal(row$TRAV, "TRAV10") && safe_equal(row$TRAJ, "TRAJ30") &&
      safe_equal(row$TRBV, "TRBV6-5") && safe_equal(row$TRBJ, "TRBJ1-4") &&
      is_blank(row$TRGV) && is_blank(row$TRGJ)&& 
      is_blank(row$TRDV) && is_blank(row$TRDJ)) {
    full_metadata[i, c("color", "shape")] <- c("#b45e3d", "circle")
    match_counts[7] <- match_counts[7] + 1
    next
  }
  
  #8
  if (is_blank(row$TRDV) && is_blank(row$TRDJ) && is_blank(row$TRAV) && is_blank(row$TRAJ) &&
      is_blank(row$TRBV) && is_blank(row$TRBJ) &&
      safe_equal(row$TRGV, "TRGV3") && safe_equal(row$TRGJ, "TRGJ1")) {
    full_metadata[i, c("color", "shape")] <- c("#9aaf9e", "triangle")
    match_counts[8] <- match_counts[8] + 1
    next
  }
  #9
  if (is_blank(row$TRDV) && is_blank(row$TRDJ) && is_blank(row$TRAV) && is_blank(row$TRAJ) &&
      is_blank(row$TRBV) && is_blank(row$TRBJ) &&
      safe_equal(row$TRGV, "TRGV8") && safe_equal(row$TRGJ, "TRGJP2")) {
    full_metadata[i, c("color", "shape")] <- c("#ca563f", "triangle")
    match_counts[9] <- match_counts[9] + 1
    next
  }
  #10
  if (is_blank(row$TRDV) && is_blank(row$TRDJ) && is_blank(row$TRAV) && is_blank(row$TRAJ) &&
      is_blank(row$TRBV) && is_blank(row$TRBJ) &&
      safe_equal(row$TRGV, "TRGV3") && safe_equal(row$TRGJ, "TRGJP2")) {
    full_metadata[i, c("color", "shape")] <- c("#f4d3c2", "triangle")
    match_counts[10] <- match_counts[10] + 1
    next
  }
  #11
  if (is_blank(row$TRDV) && is_blank(row$TRDJ) && is_blank(row$TRAV) && is_blank(row$TRAJ) &&
      is_blank(row$TRBV) && is_blank(row$TRBJ) &&
      safe_equal(row$TRGV, "TRGV10") && safe_equal(row$TRGJ, "TRGJ1")) {
    full_metadata[i, c("color", "shape")] <- c("#e9d41f", "triangle")
    match_counts[11] <- match_counts[11] + 1
    next
  }
  
  # شرط 12 (پیش‌فرض)
  full_metadata[i, c("color", "shape")] <- c("#c7c7c7", "circle")
  match_counts[12] <- match_counts[12] + 1
}

# چاپ تعداد دفعاتی که هر شرط برقرار بوده
for (j in 1:12) {
  cat(paste("شرط", j, ":", match_counts[j], "بار درست بود\n"))
}







library(ggplot2)
library(dplyr)

# فقط ردیف‌هایی که رنگشان خاکستری نیست را برای legend نگه می‌داریم
legend_df <- full_metadata %>%
  filter(color != "#c7c7c7") %>%
  mutate(clone_label = paste0("Clone ", as.numeric(factor(paste(color, shape)))))  # ساخت یک برچسب یکتا برای legend

# ترکیب دیتا برای داشتن legend درست (بقیه به عنوان 'other')
full_metadata <- full_metadata %>%
  left_join(
    legend_df %>% select(color, shape, clone_label) %>% distinct(),
    by = c("color", "shape")
  ) %>%
  mutate(clone_label = ifelse(is.na(clone_label), "Other", clone_label))

# رسم UMAP
p <- ggplot(full_metadata, aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2)) +
  geom_point(aes(color = clone_label, shape = clone_label), size = 1, alpha = 0.8) +
  scale_color_manual(
    values = legend_df %>% select(clone_label, color) %>% distinct() %>% deframe()
  ) +
  scale_shape_manual(
    values = legend_df %>% select(clone_label, shape) %>%
      distinct() %>%
      mutate(shape = ifelse(shape == "circle", 16, 17)) %>%  # 16: circle, 17: triangle
      deframe()
  ) +
  ggtitle("UMAP of Clonotypes") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # عنوان وسط‌چین
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  )

# ذخیره با کیفیت بالا
ggsave("UMAP_Clonotypes.png", plot = p, width = 8, height = 6, dpi = 300)