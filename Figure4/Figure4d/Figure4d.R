# ============================================================
# Component decomposition boxplot with paired significance brackets and stars
# Only reads saved site_DO_WT_decomposition_all_hours.csv
# ============================================================

rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(tidyverse)
})

# ============================================================
# 1. Paths
# ============================================================

out_dir <- "D:/Python/R/6 DO-WT/需要上传的数据和代码/Figure4/Figure4d"

decomp_path <- file.path(out_dir, "site_DO_WT_decomposition_all_hours.csv")

fig_dir <- file.path(out_dir, "plot_only_component_boxplot")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 2. Read saved decomposition result
# ============================================================

site_decomp_all <- read_csv(
  decomp_path,
  show_col_types = FALSE,
  guess_max = 100000
) %>%
  mutate(
    beta_obs = as.numeric(beta_obs),
    beta_sat = as.numeric(beta_sat),
    beta_exc = as.numeric(beta_exc)
  )

cat("\nLoaded decomposition rows:", nrow(site_decomp_all), "\n")
cat("Valid beta_obs:", sum(is.finite(site_decomp_all$beta_obs)), "\n")
cat("Valid beta_sat:", sum(is.finite(site_decomp_all$beta_sat)), "\n")
cat("Valid beta_exc:", sum(is.finite(site_decomp_all$beta_exc)), "\n")

# ============================================================
# 3. Prepare plotting data
# ============================================================

# Retain one complete triplet per site for plotting and paired tests.
paired_df <- site_decomp_all %>%
  select(file_name, beta_sat, beta_exc, beta_obs) %>%
  distinct() %>%
  filter(
    !is.na(file_name),
    is.finite(beta_sat), is.finite(beta_exc), is.finite(beta_obs)
  )
if (anyDuplicated(paired_df$file_name)) {
  stop("同一 file_name 存在多条不同的分解记录，请先确认每站唯一记录。")
}
if (nrow(paired_df) < 2) stop("至少需要两个具有完整三分量的站点。")
cat("Complete paired sites:", nrow(paired_df), "\n")

component_keys <- c("beta_sat", "beta_exc", "beta_obs")
component_labels <- c(
  "Solubility-related\nDO change",
  "Non-solubility \nrelated DO change",
  "Observed\nDO change"
)

# Three two-sided paired Wilcoxon signed-rank tests.
pairs <- combn(component_keys, 2, simplify = FALSE)
sig_results <- purrr::map_dfr(pairs, function(pair) {
  x <- paired_df[[pair[1]]]
  y <- paired_df[[pair[2]]]
  p_val <- if (all(x == y)) {
    1
  } else {
    wilcox.test(
      x, y, paired = TRUE, alternative = "two.sided",
      exact = FALSE, correct = TRUE
    )$p.value
  }
  tibble(group1 = pair[1], group2 = pair[2], n_pairs = length(x), p = p_val)
}) %>%
  mutate(p_adj = p.adjust(p, method = "BH"))
if (any(!is.finite(sig_results$p_adj))) stop("检验未返回有效的 p 值。")
print(sig_results)

# Stars based on BH-adjusted p values.
sig_results <- sig_results %>%
  mutate(
    sig_label = case_when(
      p_adj < 0.001 ~ "***",
      p_adj < 0.01 ~ "**",
      p_adj < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    x1 = match(group1, component_keys),
    x2 = match(group2, component_keys)
  ) %>%
  arrange(x2 - x1, x1) %>%
  mutate(level = row_number())
write_csv(sig_results, file.path(fig_dir, "Component_paired_Wilcoxon_BH.csv"))

slope_long_all <- paired_df %>%
  select(file_name, beta_sat, beta_exc, beta_obs) %>%
  pivot_longer(
    cols = c(beta_sat, beta_exc, beta_obs),
    names_to = "component",
    values_to = "slope"
  ) %>%
  mutate(
    component = factor(
      component,
      levels = c("beta_sat", "beta_exc", "beta_obs"),
      labels = c(
        "Solubility-related\nDO change",
        "Non-solubility \nrelated DO change",
        "Observed\nDO change"
      )
    )
  ) %>%
  filter(is.finite(slope))

# ============================================================
# 4. Theme
# ============================================================

theme_pub <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    theme(
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      axis.line = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 100, r = 10, b = 5, l = 5)
    )
}

# ============================================================
# 5. Plot
# ============================================================

# ----------------------------

box_fill_color <- "#CFC3D6"   # 盒子蓝色，可改成 "#2C7FB8"

# 纵坐标范围
# 设为 NULL 就自动按数据范围显示
# 例如：y_lim_manual <- c(-1.5, 3.5)
y_lim_manual <- c(-0.5, 2)

# 纵坐标刻度
# 例如 seq(-1, 3, by = 1)
y_breaks_manual <- seq(-0.5, 2, by = 0.5)

# 字体大小
x_text_size <- 17
y_text_size <- 18
y_title_size <- 18

# 横坐标文字角度
x_text_angle <- 0
x_text_hjust <- 0.5

# ----------------------------
# 如果不手动设置 y 轴范围，则自动计算
# ----------------------------

if (is.null(y_lim_manual)) {
  y_lim_use <- quantile(
    slope_long_all$slope,
    probs = c(0.01, 0.99),
    na.rm = TRUE
  )
} else {
  y_lim_use <- y_lim_manual
}

# ----------------------------
# 绘图
# ----------------------------

# Keep 12% padding within the panel; brackets are above its upper edge.
y_span <- diff(y_lim_use)
sig_results <- sig_results %>%
  mutate(
    y = y_lim_use[2] + (0.06 + (level - 1) * 0.08) * y_span,
    y_tip = y - 0.04 * y_span,
    star_y = y - 0.02 * y_span
  )

p_decomp <- ggplot(slope_long_all, aes(x = component, y = slope)) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.7
  ) +
  geom_boxplot(
    outlier.shape = NA,
    width = 0.55,
    fill = box_fill_color,
    color = "black",
    linewidth = 0.85,
    alpha = 0.85
  ) +
  geom_segment(
    data = sig_results,
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE, linewidth = 0.8, color = "black"
  ) +
  geom_segment(
    data = sig_results,
    aes(x = x1, xend = x1, y = y_tip, yend = y),
    inherit.aes = FALSE, linewidth = 0.8, color = "black"
  ) +
  geom_segment(
    data = sig_results,
    aes(x = x2, xend = x2, y = y_tip, yend = y),
    inherit.aes = FALSE, linewidth = 0.8, color = "black"
  ) +
  geom_text(
    data = sig_results,
    aes(x = (x1 + x2) / 2, y = star_y, label = sig_label),
    inherit.aes = FALSE,
    size = 8, fontface = "bold", color = "black", vjust = 0
  ) +
  coord_cartesian(ylim = y_lim_use, clip = "off") +
  scale_y_continuous(
    breaks = y_breaks_manual,
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    x = NULL,
    y = expression("Slope with WT (mg L"^-1*" "*degree*C^-1*")")
  ) +
  theme_pub(base_size = 14) +
  theme(
    axis.text.x = element_text(
      size = x_text_size,
      color = "black",
      angle = x_text_angle,
      hjust = x_text_hjust
    ),
    axis.text.y = element_text(
      size = y_text_size,
      color = "black"
    ),
    axis.title.y = element_text(
      size = y_title_size,
      color = "black"
    )
  )

print(p_decomp)

# ============================================================
# 6. Save
# ============================================================

ggsave(
  filename = "Component_decomposition_boxplot_stars.png",
  plot = p_decomp,
  width = 7,
  height = 5,
  dpi = 600,
  bg = "white"
)

message("\nFinished. Figure saved to: ", fig_dir)