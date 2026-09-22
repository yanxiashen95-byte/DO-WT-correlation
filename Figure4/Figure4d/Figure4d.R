# ============================================================
# Plot-only code: component decomposition boxplot
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

out_dir <- "D:/Python/R/6 DO-WT/revised_Figure4/拆分WT和solar/DO_WT_decomposition_output"

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

slope_long_all <- site_decomp_all %>%
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
        "Solubility\ncomponent",
        "Non-solubility\n-related DO",
        "Observed\nDO response"
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
      plot.margin = margin(5, 5, 5, 5)
    )
}

# ============================================================
# 5. Plot
# ============================================================

# ----------------------------

box_fill_color <- "#6BA3D6"   # 盒子蓝色，可改成 "#2C7FB8"

# 纵坐标范围
# 设为 NULL 就自动按数据范围显示
# 例如：y_lim_manual <- c(-1.5, 3.5)
y_lim_manual <- c(-0.5, 2)

# 纵坐标刻度
# 例如 seq(-1, 3, by = 1)
y_breaks_manual <- seq(-0.5, 2, by = 0.5)

# 字体大小
x_text_size <- 18
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
  coord_cartesian(
    ylim = y_lim_use
  ) +
  scale_y_continuous(
    breaks = y_breaks_manual
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
  file.path(fig_dir, "Component_decomposition_boxplot.png"),
  p_decomp,
  width = 6.2,
  height = 4,
  dpi = 600,
  bg = "white"
)

message("\nFinished. Figure saved to: ", fig_dir)