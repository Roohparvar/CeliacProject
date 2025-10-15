
library(magick)

# مسیر دایرکتوری تصاویر
img_dir <- "C:/Esmaeil/CeliacProject/CeliacProject/5_TCR gene pairing/γδ/All in One"  # <-- مسیر دایرکتوری خودت

# نصب بسته اگر نصب نشده
if(!require(magick)) install.packages("magick")
library(magick)

# مسیر دایرکتوری تصاویر


# تعداد ستون و ردیف
cols <- 6
rows <- 4

# لیست تصاویر
img_files <- paste0(img_dir, "/", 1:24, ".png")

# خواندن تصاویر
images <- lapply(img_files, image_read)

# نام ستون‌ها و ردیف‌ها
col_names <- c(
  "All Tγδ Clusters",
  "Just Act. Tγδ",
  "Just NK Tγδ",
  "Just Trm IEL",
  "Just Tγδ CD8+",
  "Just Tγδ INSIG1+"
)
row_names <- c("Healthy", "ACD", "RCD-I", "RCD-II")

# تبدیل هر ستون به یک تصویر ترکیبی عمودی
columns <- list()
for (i in 1:cols) {
  start_idx <- (i-1)*rows + 1
  end_idx <- i*rows
  column_imgs <- images[start_idx:end_idx]
  
  # اضافه کردن متن ردیف‌ها روی هر تصویر
  for (j in 1:rows) {
    column_imgs[[j]] <- image_annotate(
      column_imgs[[j]],
      text = row_names[j],
      gravity = "NorthWest",
      location = "+10+10",
      size = 25,
      color = "black",
      font = "Arial"
    )
  }
  
  # ستون عمودی
  columns[[i]] <- image_append(image_join(column_imgs), stack = TRUE)
  
  # اضافه کردن نام ستون بالای ستون
  columns[[i]] <- image_annotate(
    columns[[i]],
    text = col_names[i],
    gravity = "North",
    size = 35,
    color = "black",
    font = "Arial",
    location = "+0+10"
  )
}

# حالا همه ستون‌ها را کنار هم افقی بچسبان
final_img <- image_append(image_join(columns), stack = FALSE)

# نمایش تصویر نهایی


# ذخیره تصویر نهایی با بالاترین کیفیت
# PNG
image_write(final_img, path = paste0(img_dir, "/combined_with_labels.png"), format = "png", density = 600)

# PDF
image_write(final_img, path = paste0(img_dir, "/combined_with_labels.pdf"), format = "pdf", density = 600)
