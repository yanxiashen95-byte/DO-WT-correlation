data <- read_csv("Australia_avg.csv", show_col_types = FALSE)


ggplot(data, aes(x = WT, y = DO)) +
  geom_point(
    color = "grey70",
    size = 14, shape = 16  # 实心圆点
  ) +
  geom_smooth(method = "lm", se = FALSE, color = "#2166ac", size = 5) +
  labs(
    title = "",
    x = "WT (°C)",
    y = "DO (mg/L)"
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 2
    ),
    axis.text = element_text(
      size = 40,
      color = "black",
      # face = "bold"
    ),
    axis.ticks = element_line(
      color = "black",
      linewidth = 1.5
    ),
    axis.ticks.length = unit(0.25, "cm"),
    axis.title = element_text(
      size = 40,
      #face = "bold"
    ),
    plot.title = element_text(
      size = 40,
      #face = "bold",
      hjust = 0.5
    )
  )

ggsave(
  filename = "Australia_散点图.png",  # 输出文件名
  plot = last_plot(),                   # 或指定 plot = p，如果你存到了变量 p 中
  width = 10, height = 8,               # 单位：英寸（可以改大）
  dpi = 600                           # 分辨率：高清出版级                          # 背景白色（非透明）
)