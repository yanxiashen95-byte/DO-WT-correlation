library(readr)
library(dplyr)

# 1. 选择数据文件
raw_df <- read_csv(
  file.choose(),
  col_types = cols(file_name = col_character())
)

# 2. 删除完全重复的记录，保留有效昼夜配对
unique_df <- raw_df %>%
  distinct()

test_df <- unique_df %>%
  mutate(
    r_day = as.numeric(r_day),
    r_night = as.numeric(r_night)
  ) %>%
  filter(
    is.finite(r_day),
    is.finite(r_night),
    between(r_day, -1, 1),
    between(r_night, -1, 1)
  ) %>%
  mutate(delta_r = r_day - r_night)

cat("删除的完全重复行数：", nrow(raw_df) - nrow(unique_df), "\n")
cat("删除的无效配对数：", nrow(unique_df) - nrow(test_df), "\n")
cat("有效配对数：", nrow(test_df), "\n")

# 3. 描述统计
summary_df <- test_df %>%
  summarise(
    n_pairs = n(),
    median_r_day = median(r_day),
    median_r_night = median(r_night),
    median_paired_difference = median(delta_r),
    percentage_day_greater = mean(delta_r > 0) * 100
  )

print(summary_df)

# 4. 双侧配对 Wilcoxon 符号秩检验
test_result <- wilcox.test(
  test_df$r_day,
  test_df$r_night,
  paired = TRUE,
  alternative = "two.sided",
  exact = FALSE,
  correct = TRUE
)

print(test_result)

cat(
  "配对检验 p 值：",
  format(test_result$p.value, scientific = TRUE, digits = 4),
  "\n"
)