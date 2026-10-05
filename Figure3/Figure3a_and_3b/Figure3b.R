# Day–Night boxplots with a two-sided paired Wilcoxon signed-rank test
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Select the CSV containing r_day and r_night
raw_df <- read_csv(file.choose(), show_col_types = FALSE)
stopifnot(all(c("r_day", "r_night") %in% names(raw_df)))

# Use the same valid pairs for both the test and the plot.
# Remove only completely duplicated records, as in the original test script.
unique_df <- raw_df %>% distinct()
test_df <- unique_df %>%
  mutate(
    r_day = as.numeric(r_day),
    r_night = as.numeric(r_night)
  ) %>%
  filter(
    is.finite(r_day), is.finite(r_night),
    between(r_day, -1, 1), between(r_night, -1, 1)
  ) %>%
  mutate(delta_r = r_day - r_night)

if (nrow(test_df) < 2) stop("至少需要两组有效昼夜配对。")
cat("删除的完全重复行数：", nrow(raw_df) - nrow(unique_df), "\n")
cat("删除的无效配对数：", nrow(unique_df) - nrow(test_df), "\n")
cat("有效配对数：", nrow(test_df), "\n")

# 2. Two-sided paired Wilcoxon signed-rank test
test_result <- wilcox.test(
  test_df$r_day, test_df$r_night,
  paired = TRUE, alternative = "two.sided",
  exact = FALSE, correct = TRUE
)
print(test_result)
p_value <- test_result$p.value
if (!is.finite(p_value)) {
  stop("检验未得到有效 p 值，请检查配对差值是否全部为零。")
}

# A single comparison; no multiple-comparison correction is needed.
sig_label <- case_when(
  p_value < 0.001 ~ "***",
  p_value < 0.01 ~ "**",
  p_value < 0.05 ~ "*",
  TRUE ~ "ns"
)
cat("配对检验 p 值：", format(p_value, scientific = TRUE, digits = 4), "\n")
cat("图中显著性标注：", sig_label, "\n")

summary_df <- test_df %>%
  summarise(
    n_pairs = n(),
    median_r_day = median(r_day),
    median_r_night = median(r_night),
    median_paired_difference = median(delta_r),
    percentage_day_greater = mean(delta_r > 0) * 100
  )
print(summary_df)

# 3. Prepare plotting data
df_plot <- test_df %>%
  select(r_day, r_night) %>%
  pivot_longer(everything(), names_to = "period", values_to = "r") %>%
  mutate(
    period = recode(period, r_day = "Day", r_night = "Night"),
    period = factor(period, levels = c("Day", "Night"))
  )
stat_df <- df_plot %>%
  group_by(period) %>%
  summarise(median_r = median(r), .groups = "drop")

# 4. Bracket above the panel, following Figure_1b.R
# Panel spans -1.24 to 1.24 after 12% expansion of [-1, 1].
bracket_y <- 1.4
tip_y <- 1.3
star_y <- 1.35

# 5. Plot
p <- ggplot(df_plot, aes(x = period, y = r, fill = period)) +
  geom_hline(
    yintercept = 0, linetype = "dashed",
    color = "grey50", linewidth = 0.6
  ) +
  geom_boxplot(
    width = 0.48, outlier.shape = NA,
    color = "black", linewidth = 0.8, alpha = 0.85
  ) +
  geom_text(
    data = stat_df,
    aes(x = period, y = 1, label = sprintf("%.2f", median_r)),
    inherit.aes = FALSE, vjust = -0.6,
    size = 6, fontface = "bold", color = "black"
  ) +
  # Horizontal bracket and its two downward tips
  annotate("segment", x = 1, xend = 2,
           y = bracket_y, yend = bracket_y, linewidth = 0.8) +
  annotate("segment", x = 1, xend = 1,
           y = tip_y, yend = bracket_y, linewidth = 0.8) +
  annotate("segment", x = 2, xend = 2,
           y = tip_y, yend = bracket_y, linewidth = 0.8) +
  annotate("text", x = 1.5, y = star_y, label = sig_label,
           size = 7, fontface = "bold", vjust = 0) +
  scale_fill_manual(values = c("Day" = "#fdae61", "Night" = "#6BA3D6")) +
  scale_y_continuous(
    name = "Correlation (r)",
    breaks = seq(-1, 1, 0.5),
    expand = expansion(mult = c(0.15, 0.15))
  ) +
  # Set viewing limits here, rather than scale limits, so the bracket survives.
  coord_cartesian(ylim = c(-1, 1), clip = "off") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.text = element_text(size = 18, color = "black"),
    axis.title = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(t = 45, r = 10, b = 10, l = 10)
  )

print(p)

ggsave("day_night_with_significance.png", p,
       width = 3, height = 4, dpi = 300, bg = "white")

