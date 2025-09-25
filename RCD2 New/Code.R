library(readxl)
library(dplyr)
library(writexl)

# 1. Read the Excel file
df <- read_excel("EATL_tcr_combine.xlsx")

# 2. Create cdr_Full_ab = a_cdr3 + b_cdr3
df <- df %>%
  mutate(
    cdr_Full_ab = ifelse(!is.na(a_cdr3) & !is.na(b_cdr3),
                         paste0(a_cdr3, "_", b_cdr3), NA)
  )

# 3. Create cdr_Full_gd = g_cdr3 + d_cdr3
df <- df %>%
  mutate(
    cdr_Full_gd = ifelse(!is.na(g_cdr3) & !is.na(d_cdr3),
                         paste0(g_cdr3, "_", d_cdr3), NA)
  )

# 4. Create clone_size_ab = number of repeats of each clone in cdr_Full_ab
df <- df %>%
  group_by(cdr_Full_ab) %>%
  mutate(clone_size_ab = ifelse(!is.na(cdr_Full_ab), n(), NA)) %>%
  ungroup()

# 5. Create clone_size_gd = number of repeats of each clone in cdr_Full_gd
df <- df %>%
  group_by(cdr_Full_gd) %>%
  mutate(clone_size_gd = ifelse(!is.na(cdr_Full_gd), n(), NA)) %>%
  ungroup()


# 4. Compute clone size for g_cdr3 only and b_cdr3 only
df <- df %>%
  group_by(g_cdr3) %>%
  mutate(clone_size_g = ifelse(!is.na(g_cdr3), n(), NA)) %>%
  ungroup() %>%
  group_by(b_cdr3) %>%
  mutate(clone_size_b = ifelse(!is.na(b_cdr3), n(), NA)) %>%
  ungroup()




# 5. Reorder columns
df <- df %>%
  select(
    a_cdr3,
    b_cdr3,
    clone_size_b,
    cdr_Full_ab,
    clone_size_ab,
    g_cdr3,
    clone_size_g,
    d_cdr3,
    cdr_Full_gd,
    clone_size_gd,
    everything() # keep the rest of columns after these
  )


# 6. Create a summary table
summary_table <- data.frame(
  only_ab = sum(!is.na(df$cdr_Full_ab) & is.na(df$cdr_Full_gd)),
  only_gd = sum(is.na(df$cdr_Full_ab) & !is.na(df$cdr_Full_gd)),
  both    = sum(!is.na(df$cdr_Full_ab) & !is.na(df$cdr_Full_gd)),
  neither = sum(is.na(df$cdr_Full_ab) & is.na(df$cdr_Full_gd))
)








