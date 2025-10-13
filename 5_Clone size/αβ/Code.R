setwd("C:/Esmaeil/CeliacProject/CeliacProject/Clone size/αβ")

library(ggplot2)
library(scales)
library(dplyr)
library(patchwork)  # برای چیدمان چند نمودار کنار هم

# آماده‌سازی داده
plot_data <- full_metadata
plot_data$clone_size_ab <- as.numeric(as.character(plot_data$clone_size_ab))

# لیست بیماری‌ها (ترتیب دلخواه برای 2x2)
order_diag <- c("Healthy", "ACD", "RCD-I", "RCD-II")

# لیست برای ذخیره نمودارها
p_list <- list()

# حلقه روی هر بیماری
for (diag in order_diag) {
  
  # سلول‌های همان بیماری
  data_diag <- filter(plot_data, Diagnosis == diag)
  
  # فقط سلول‌های رنگی همان بیماری (مرتب شده → بزرگ‌ها top)
  data_colored <- data_diag %>%
    filter(!is.na(clone_size_ab)) %>%
    arrange(clone_size_ab)
  
  # رسم نمودار
  p <- ggplot() +
    # لایه ۱: همه سلول‌ها خاکستری پس‌زمینه
    geom_point(
      data = plot_data,
      aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2),
      color = "grey90",
      size = 0.5,
      alpha = 0.5
    ) +
    # لایه ۲: سلول‌های رنگی همان بیماری (مرتب شده → بزرگ‌ها top)
    geom_point(
      data = data_colored,
      aes(x = scVI_with_hvg_UMAP_1, y = scVI_with_hvg_UMAP_2, color = clone_size_ab),
      size = 0.8,
      alpha = 0.9
    ) +
    scale_color_viridis_c(
      name = "Clone size",
      option = "viridis",
      limits = c(1, 50),
      oob = scales::squish,
      # breaks = seq(1, 50, by = 10),       # اضافه کردن مقادیر دقیق برای نمایش
      labels = function(x) {
        x <- prettyNum(x, digits = 2)
        x[length(x)] <- paste0("≥ ", round(50, 0))
        x
      }
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      legend.title = element_text(vjust = 2),     # عنوان بالاتر
      legend.title.align = 0.5,
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(
      x = "",
      y = "",
      title = diag
    )
  
  # ذخیره نمودار جداگانه
  pdf_filename <- paste0("UMAP_highlight_", diag, ".pdf")
  png_filename <- paste0("UMAP_highlight_", diag, ".png")
  ggsave(pdf_filename, plot = p, width = 7, height = 6, bg = "white")
  ggsave(png_filename, plot = p, width = 7, height = 6, dpi = 300, bg = "white")
  
  cat("✅ Saved separate plot for:", diag, "\n")
  
  # ذخیره در لیست برای ترکیب
  p_list[[diag]] <- p
}

# ترکیب ۴ نمودار در یک تصویر 2x2
final_plot <- (p_list[["Healthy"]] | p_list[["ACD"]]) /
  (p_list[["RCD-I"]] | p_list[["RCD-II"]]) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right",
        legend.justification = "center")  # وسط سمت راست

# ذخیره تصویر نهایی
ggsave("UMAP_4_diseases.png", plot = final_plot, width = 14, height = 12, dpi = 300, bg = "white")
ggsave("UMAP_4_diseases.pdf", plot = final_plot, width = 14, height = 12, bg = "white")

cat("✅ Saved combined 4-disease UMAP plot!\n")
