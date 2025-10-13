library(dplyr)
library(packcircles)
library(ggplot2)


full_metadata <- full_metadata[full_metadata$Diagnosis == "RCD-I", ]

clean_data <- full_metadata %>%
  filter(!is.na(clone_size_gd))

clones_data <- clean_data %>%
  group_by(cluster) %>%
  summarise(mean_clone_size = mean(clone_size_gd, na.rm = TRUE)) %>%
  arrange(mean_clone_size)

packing <- circleProgressiveLayout(clones_data$mean_clone_size, sizetype = "area")
packing$x <- packing$x * 1.3
packing$y <- packing$y * 1.3

clones_data <- cbind(clones_data, packing)

# تعریف رنگ بر اساس مقدار mean_clone_size
clones_data <- clones_data %>%
  mutate(color = case_when(
    mean_clone_size == 1 ~ "#1B263B",
    mean_clone_size >= 2 & mean_clone_size <= 10 ~ "#2E4057",
    mean_clone_size >= 11 & mean_clone_size <= 50 ~ "#355C7D",
    mean_clone_size >= 51 & mean_clone_size <= 100 ~ "#3A7BD5",
    mean_clone_size > 100 ~ "#1E90FF",
    TRUE ~ "#CCCCCC"
  ))

dat.gg <- circleLayoutVertices(packing, npoints = 50)

# به dat.gg رنگ ها رو اضافه کنیم
# dat.gg$id شماره ردیف در clones_data هست، پس میتونیم رنگ‌ها رو متصل کنیم
dat.gg <- dat.gg %>%
  left_join(clones_data %>% mutate(id = row_number()) %>% select(id, color), by = "id")

clones_data <- clones_data %>%
  mutate(font_size = radius * 1.7)  # ضریب 3 رو می‌تونی کم یا زیاد کنی

p <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill = color), color = "black", alpha = 0.6) +
  geom_text(data = clones_data, aes(x, y, label = cluster, size = font_size)) +
  scale_fill_identity() + 
  scale_size_identity() +   
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Ball-packing plot of TCRγδ Clusters by clonal expansion") +
  coord_fixed() +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.4))) +  # فضای بیشتر سمت راست
  scale_y_continuous(expand = expansion(mult = c(0.1, 0.1)))



# دیتافریم لگند بر اساس محدوده های رنگی تعریف شده
# اضافه کردن ردیف عنوان به دیتافریم لگند
# دیتافریم لگند اصلی
offset_y <- -5

legend_data <- data.frame(
  label = c("1", "2-10", "11-50", "51-100", ">100"),
  color = c("#1B263B", "#2E4057", "#355C7D", "#3A7BD5", "#1E90FF"),
  x = rep(max(clones_data$x) + 8, 5),
  y = seq(from = max(clones_data$y), to = max(clones_data$y) - 6, length.out = 5) + offset_y,
  size = c(4, 5, 6, 7, 8)
)

legend_title <- data.frame(
  label = "TCRγδ Clone Size",
  x = legend_data$x[1] + 0,
  y = legend_data$y[1] + 6.3 + offset_y
)

p <- p + 
  geom_point(data = legend_data, aes(x = x, y = y, fill = color, size = size), shape = 21, color = "black") + 
  geom_text(data = legend_data, aes(x = x + 1, y = y, label = label), hjust = 0, size = 4) +
  geom_text(data = legend_title, aes(x = x, y = y, label = label), hjust = 0.5, size = 5) + 
  scale_fill_identity() +
  scale_size_identity()



ggsave("ball_packing_plot.png", plot = p, width = 9, height = 6, dpi = 900, bg = "white")
