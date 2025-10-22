library(dplyr)
library(writexl)

# فیلتر کردن ردیف‌ها بر اساس شرایط موردنظر
filtered_metadata <- full_metadata %>%
  filter(TRAV == "TRAV26-1",TRAJ == "TRAJ17", TRBV == "TRBV6-5", TRBJ == "TRBJ1-4")

# ذخیره در یک فایل اکسل
write_xlsx(filtered_metadata, "filtered_TRBV6-5_TRBJ1-4.xlsx")