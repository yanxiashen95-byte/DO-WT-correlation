library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# === 读取数据 ===
df <- read_csv("美国_avg.csv", show_col_types = FALSE)

# === 统一列名（防止空格问题）===
names(df) <- tolower(trimws(names(df)))

# === 设置范围和缩放 WT 到 DO 范围 ===
range_DO <- range(df$do, na.rm = TRUE)
range_WT <- range(df$wt, na.rm = TRUE)

df <- df %>%
  mutate(WT_scaled = rescale(wt, to = range_DO))

# === 绘图 ===
ggplot(df, aes(x = year)) +
  geom_line(aes(y = do, color = "DO"), size = 6) +
  geom_line(aes(y = WT_scaled, color = "WT"), size = 6) +
  
  scale_x_continuous(
    name = "Year",
    breaks = pretty(df$year, n = 5),  # 可自定义年间隔
    expand = expansion(mult = c(0, 0))  # ✅ 只延长右边
  ) +
  
  scale_y_continuous(
    name = "DO (mg/L)",
    breaks = pretty(range_DO, n = 4),
    sec.axis = sec_axis(
      ~ rescale(., from = range_DO, to = range_WT),
      name = "WT (°C)",
      breaks = pretty(range_WT, n = 4),
      labels = function(x) format(x, nsmall = 1)
    )
  ) +
  
  scale_color_manual(
    values = c("DO" = "#998ec3", "WT" = "#e08214"),
    labels = c("DO", "WT")
  ) +
  
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", size = 3),
    axis.title.x = element_text(size = 40,face = "bold"),
    axis.text.x = element_text(size = 40,face = "bold"),
    axis.title.y.left = element_text(color = "#998ec3", size = 40, face = "bold"),
    axis.text.y.left = element_text(color = "#998ec3", size = 40,face = "bold"),
    axis.title.y.right = element_text(color = "#e08214", size = 40, face = "bold"),
    axis.text.y.right = element_text(color = "#e08214", size = 40,face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 22),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  ) +
  labs(x = "Year", title = "Annual Mean DO and WT", color = NULL)


ggsave(
  filename = "美国_trend2.png",  # 输出文件名
  plot = last_plot(),                   # 或指定 plot = p，如果你存到了变量 p 中
  width = 12, height = 8,               # 单位：英寸（可以改大）
  dpi = 600,                            # 分辨率：高清出版级别
  bg = "white"                          # 背景白色（非透明）
)