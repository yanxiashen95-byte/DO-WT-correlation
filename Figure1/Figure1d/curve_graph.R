library(ggplot2)
library(dplyr)
library(scales)
library(readr)

# === 读取数据（你实际的数据文件）===
df <- read_csv("奥地利.csv") %>%
  mutate(
    index = row_number(),
    # 自动识别时间格式（即便有秒、斜杠、横杠也能完美兼容，并截断到分钟）
    timestamp = lubridate::ymd_hms(timestamp, truncated = 2, tz = "UTC"),
    
    # 确保 WT 和 DO 转换为数值型（防止原始数据有空字符导致转换出问题）
    WT = as.numeric(WT),
    DO = as.numeric(DO),
    WT_scaled = scales::rescale(WT, to = range(DO, na.rm = TRUE))
  ) %>%
  # 安全起见，过滤掉时间为 NA 的行
  filter(!is.na(timestamp))

range_DO <- range(df$DO, na.rm = TRUE)
range_WT <- range(df$WT, na.rm = TRUE)

# ==== 生成夜间时间段 ====
#dates <- unique(as.Date(df$timestamp))  # 提取所有日期
#night_df <- bind_rows(
# 晚上 20:00 - 23:59
#  data.frame(
#   start = as.POSIXct(paste(dates, "20:00:00"), tz = "UTC"),
#   end   = as.POSIXct(paste(dates, "23:59:59"), tz = "UTC")
# ),
# 凌晨 00:00 - 07:00 （结束在第二天）
# data.frame(
#   start = as.POSIXct(paste(dates, "00:00:00"), tz = "UTC"),
#    end   = as.POSIXct(paste(dates, "07:00:00"), tz = "UTC")
# )
#)

# ==== 绘图 ====
# 当天零点
day_start <- lubridate::floor_date(min(df$timestamp), unit = "day")

# 固定刻度：00:00、06:00、12:00、18:00
time_breaks <- day_start + lubridate::hours(c(0, 12, 24))

ggplot(df, aes(x = timestamp)) +
  # 灰色夜间区块
  #geom_rect(
  #  data = night_df,
  #   inherit.aes = FALSE,
  #   aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
  #   fill = "grey80", alpha = 0.5
  #) +
  
  # DO 和 WT 曲线
  geom_line(aes(y = DO, color = "DO"), size = 10,linetype = "dashed") +
  geom_line(aes(y = WT_scaled, color = "WT"), size = 10,linetype = "dashed") +
  
  scale_x_datetime(
    name = "",
    breaks = time_breaks,
    date_labels = "%H:%M",
    expand = expansion(mult = c(0.12, 0.12))
  )  +
  scale_y_continuous(
    name = "DO (mg/L)",
    breaks = pretty(range_DO, n = 2),
    expand = expansion(mult = c(0.12, 0.12)),
    sec.axis = sec_axis(
      ~ scales::rescale(., from = range_DO, to = range_WT),
      name = "WT (°C)",
      breaks = pretty(range_WT, n = 2),
      labels = function(x) format(x, nsmall = 1)
    )
  )  +
  scale_color_manual(
    values = c("DO" = "#998ec3", "WT" = "#e08214"),
    labels = c("DO", "WT")
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", size = 3),
    axis.title.x = element_text(size = 45),
    axis.text.x = element_text(size = 45, face = "bold"),
    axis.title.y.left = element_text(color = "#998ec3", size = 45, face = "bold"),
    axis.text.y.left = element_text(color = "#998ec3", size = 45, face = "bold"),
    axis.title.y.right = element_text(color = "#e08214", size = 45, face = "bold"),
    axis.text.y.right = element_text(color = "#e08214", size = 45, face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
  ) +
  labs(title = "DO and WT Time Series", color = NULL)

ggsave("奥地利.png", width = 12, height = 8, dpi = 600, bg = "white")
