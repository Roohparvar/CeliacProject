library(magick)

# مسیر دایرکتوری تصاویر
img_dir <- "C:/Esmaeil/CeliacProject/CeliacProject/6_TCR genotype distribution/γδ/Generating one figure for all"

# تعداد ستون و ردیف
cols <- 6
rows <- 4

# لیست تصاویر
img_files <- paste0(img_dir, "/", 1:24, ".png")

# خواندن تصاویر
images <- lapply(img_files, image_read)

# -----------------------
# ساخت ستون‌ها با عنوان
# -----------------------

column_titles <- c(
  "All Tγδ Clusters",
  "Just Act. Tγδ",
  "Just NK Tγδ",
  "Just Trm IEL",
  "Just Tγδ CD8+",
  "Just Tγδ INSIG1+"
)

columns <- list()
for (i in 1:cols) {
  start_idx <- (i-1)*rows + 1
  end_idx <- i*rows
  column_imgs <- images[start_idx:end_idx]
  
  # ساخت ستون عمودی
  col_img <- image_append(image_join(column_imgs), stack = TRUE)
  
  # ساخت عنوان ستون
  title_img <- image_blank(width = image_info(col_img)$width, height = 200, color = "white") %>%
    image_annotate(
      #text = paste0("Column ", i),
      text = column_titles[i],   # اینجا عنوان اختصاصی هر ستون
      size = 200,
      color = "black",
      gravity = "center"
    )
  
  # چسباندن عنوان و ستون باهم
  col_with_title <- image_append(image_join(list(title_img, col_img)), stack = TRUE)
  columns[[i]] <- col_with_title
}

# ترکیب نهایی همه ستون‌ها
final_img <- image_append(image_join(columns), stack = FALSE)

# -----------------------
# ساخت legend خیلی بزرگ
# -----------------------

legend_items <- c(
  "TRDV1"  = "#c6ab52", 
  "TRDV2"  = "#679966",   
  "TRDV3"  = "#ff6766",    
  "TRGV2"  = "#ae1f29",
  "TRGV3"  = "#e05b48",
  "TRGV4"  = "#f1a284",
  "TRGV5"  = "#fbdbc3",
  #"TRGV5P" = "#f7f7f7",
  "TRGV8"  = "#cce5f6",
  "TRGV9"  = "#8fc3dd",
  "TRGV10" = "#4790be",
  "TRGV11" = "#1666aa"
)






legend_imgs <- list()
for (i in seq_along(legend_items)) {
  color <- legend_items[i]
  name <- names(legend_items)[i]
  
  color_box <- image_blank(width = 240, height = 240, color = color)
  text_img <- image_blank(width = 1000, height = 240, color = "white") %>%
    image_annotate(name, size = 180, color = "black", gravity = "west", location = "+80+0")
  
  legend_row <- image_append(c(color_box, text_img))
  legend_imgs[[i]] <- legend_row
}

legend_final <- image_append(image_join(legend_imgs), stack = TRUE)

# -----------------------
# تراز وسط راست legend
# -----------------------

main_h <- image_info(final_img)$height
legend_h <- image_info(legend_final)$height

top_space <- image_blank(width = image_info(legend_final)$width, 
                         height = floor((main_h - legend_h) / 2), 
                         color = "white")
bottom_space <- image_blank(width = image_info(legend_final)$width, 
                            height = main_h - legend_h - image_info(top_space)$height, 
                            color = "white")

legend_centered <- image_append(image_join(list(top_space, legend_final, bottom_space)), stack = TRUE)

# فاصله سفید بین عکس و legend
space <- image_blank(width = 400, height = main_h, color = "white")

# چسباندن legend در سمت راست
final_with_legend <- image_append(c(final_img, space, legend_centered), stack = FALSE)

# ذخیره نهایی
# image_write(final_with_legend, path = paste0(img_dir, "/combined_with_legend_and_titles.png"), format = "png", density = 600)
# image_write(final_with_legend, path = paste0(img_dir, "/combined_with_legend_and_titles.pdf"), format = "pdf", density = 600)




























# -----------------------
# اضافه کردن نام ردیف‌ها سمت چپ
# -----------------------

# -----------------------
# اضافه کردن نام ردیف‌ها سمت چپ با فاصله بیشتر
# -----------------------

row_names <- c("Healthy", "ACD", "RCD1", "RCD2")

# افزایش عرض ستون متن
text_width <- 600 
left_space <- image_blank(width = 250, height = image_info(final_img)$height, color = "white")  # فاصله بیشتر
row_height <- image_info(images[[1]])$height


row_text_imgs <- list()
for (i in 1:rows) {
  txt_img <- image_blank(width = text_width, height = row_height, color = "white") %>%
    image_annotate(row_names[i], size = 180, color = "black", gravity = "center")
  row_text_imgs[[i]] <- txt_img
}

# ساخت ستون متن با چسباندن ردیف‌ها
row_column <- image_append(image_join(row_text_imgs), stack = TRUE)

# افزایش فاصله بین متن و ستون‌ها
left_space <- image_blank(width = 200, height = image_info(final_img)$height, color = "white")  # قبلا 100 بود

# ترکیب نهایی با ستون نام ردیف‌ها در سمت چپ
final_img_with_row_names <- image_append(c(row_column, left_space, final_img), stack = FALSE)


# -----------------------
# چسباندن legend مثل قبل
# -----------------------

# فاصله سفید بین عکس و legend
space <- image_blank(width = 400, height = image_info(final_img_with_row_names)$height, color = "white")

# تراز وسط راست legend
main_h <- image_info(final_img_with_row_names)$height
legend_h <- image_info(legend_final)$height

top_space <- image_blank(width = image_info(legend_final)$width, 
                         height = floor((main_h - legend_h) / 2), 
                         color = "white")
bottom_space <- image_blank(width = image_info(legend_final)$width, 
                            height = main_h - legend_h - image_info(top_space)$height, 
                            color = "white")

legend_centered <- image_append(image_join(list(top_space, legend_final, bottom_space)), stack = TRUE)

# چسباندن legend در سمت راست
final_with_legend <- image_append(c(final_img_with_row_names, space, legend_centered), stack = FALSE)

# ذخیره نهایی
image_write(final_with_legend, path = paste0(img_dir, "/combined_with_legend_row_names.png"), format = "png", density = 600)
image_write(final_with_legend, path = paste0(img_dir, "/combined_with_legend_row_names.pdf"), format = "pdf", density = 600)



