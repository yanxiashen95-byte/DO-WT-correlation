#============================改成盒图 ====================
library(dplyr)
library(tidyr)
library(ggplot2)

# -------------------------
# 1) 整理数据
# -------------------------
df_plot <- df %>%
  mutate(
    r_day   = as.numeric(r_day),
    r_night = as.numeric(r_night)
  ) %>%
  filter(!is.na(r_day), !is.na(r_night)) %>%
  select(r_day, r_night) %>%
  pivot_longer(
    everything(),
    names_to = "period",
    values_to = "r"
  ) %>%
  mutate(
    period = recode(period, r_day = "Day", r_night = "Night"),
    period = factor(period, levels = c("Day", "Night"))
  )

# -------------------------
# 2) 计算中位数用于标注
# -------------------------
stat_df <- df_plot %>%
  group_by(period) %>%
  summarise(
    median_r = median(r, na.rm = TRUE),
    q75_r    = quantile(r, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------
# 3) 颜色配置
# -------------------------
day_color   <- "#fdae61"
night_color <- "#6BA3D6"

# -------------------------
# 4) 绘制盒图
# -------------------------
p <- ggplot(df_plot, aes(x = period, y = r, fill = period)) +
  
  # 0刻度虚线
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey50",
    linewidth = 0.6
  ) +
  
  # 盒图
  geom_boxplot(
    width = 0.48,
    outlier.shape = NA,
    color = "black",
    linewidth = 0.8,
    alpha = 0.85
  ) +
  
  # 中位数标注
  geom_text(
    data = stat_df,
    aes(
      x = period,
      y = q75_r,
      label = sprintf("%.2f", median_r)
    ),
    inherit.aes = FALSE,
    vjust = -0.6,
    size = 6,
    fontface = "bold",
    color = "black"
  ) +
  
  # 坐标轴与颜色
  scale_fill_manual(
    values = c(
      "Day" = day_color,
      "Night" = night_color
    )
  ) +
  scale_y_continuous(
    name = "Correlation (r)",
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.5)
  ) +
  
  # 主题
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text = element_text(size = 18, color = "black"),
    axis.title = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.line = element_blank()
  )

print(p)

ggsave("day_night_blue.png", p, width = 3, height = 4, dpi = 300)
