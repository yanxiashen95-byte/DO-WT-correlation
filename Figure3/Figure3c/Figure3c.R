
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(tibble)

input_dir <- "D:/Python/R/6 DO-WT/revised_Figure3/r_with_flow_season"

out_dir <- file.path(input_dir, "boxplot_daynight_flow_raw")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

read_one_file <- function(file_path) {
  
  file_name <- basename(file_path)
  file_station_id <- tools::file_path_sans_ext(file_name)
  
  df <- read_csv(file_path, show_col_types = FALSE)
  names(df) <- str_trim(names(df))
  
  if (!all(c("station_id", "date", "period", "r", "flow_season") %in% names(df))) {
    warning(paste0("文件缺少必要列，已跳过：", file_name))
    return(tibble())
  }
  
  df %>%
    mutate(
      station_id = as.character(station_id),
      station_id = ifelse(is.na(station_id) | station_id == "", file_station_id, station_id),
      period = as.character(period),
      flow_season = as.character(flow_season),
      r = as.numeric(r),
      source_file = file_name
    ) %>%
    filter(
      period %in% c("daytime", "nighttime"),
      flow_season %in% c("low_flow", "normal_flow", "high_flow"),
      is.finite(r),
      r >= -1,
      r <= 1
    )
}

csv_files <- list.files(
  input_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE
)

csv_files <- csv_files[
  !str_detect(
    basename(csv_files),
    "all_sites|debug|unmatched|summary"
  )
]

all_data <- map_dfr(csv_files, read_one_file)

cat("读取到的有效日尺度记录数：", nrow(all_data), "\n")
cat("站点数量：", n_distinct(all_data$station_id), "\n")

plot_data <- all_data %>%
  mutate(
    flow_group = recode(
      flow_season,
      "low_flow" = "Low-flow",
      "normal_flow" = "Normal-flow",
      "high_flow" = "High-flow"
    ),
    period_group = recode(
      period,
      "daytime" = "Daytime",
      "nighttime" = "Nighttime"
    ),
    flow_group = factor(
      flow_group,
      levels = c("Low-flow", "Normal-flow", "High-flow")
    ),
    period_group = factor(
      period_group,
      levels = c("Daytime", "Nighttime")
    )
  )


summary_table <- plot_data %>%
  group_by(flow_group, period_group) %>%
  summarise(
    n_records = n(),
    n_sites = n_distinct(station_id),
    mean_r = mean(r, na.rm = TRUE),
    median_r = median(r, na.rm = TRUE),
    q25 = quantile(r, 0.25, na.rm = TRUE),
    q75 = quantile(r, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 1. 按站点、日期和流量组匹配昼夜记录
# ============================================================

daily_pairs <- plot_data %>%
  group_by(station_id, date, flow_group, period_group) %>%
  summarise(
    r = median(r, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = period_group,
    values_from = r
  ) %>%
  filter(
    is.finite(Daytime),
    is.finite(Nighttime)
  )

# 每站在各流量组内的中位数，使用相同的配对日期
station_pairs <- daily_pairs %>%
  group_by(station_id, flow_group) %>%
  summarise(
    r_day = median(Daytime),
    r_night = median(Nighttime),
    n_paired_days = n(),
    .groups = "drop"
  )

# ============================================================
# 2. 各流量组的双侧配对 Wilcoxon 检验
# ============================================================

sig_df <- station_pairs %>%
  group_by(flow_group) %>%
  summarise(
    n_pairs = n(),
    p = {
      if (n() < 2) {
        NA_real_
      } else if (all(r_day == r_night)) {
        1
      } else {
        wilcox.test(
          r_day,
          r_night,
          paired = TRUE,
          alternative = "two.sided",
          exact = FALSE,
          correct = TRUE
        )$p.value
      }
    },
    .groups = "drop"
  ) %>%
  mutate(
    p_adj = p.adjust(p, method = "BH"),
    sig_label = case_when(
      is.na(p_adj)   ~ "NA",
      p_adj < 0.001  ~ "***",
      p_adj < 0.01   ~ "**",
      p_adj < 0.05   ~ "*",
      TRUE           ~ "ns"
    ),
    
    # 与 position_dodge(width = 0.75) 的盒子中心一致
    x_center = as.numeric(flow_group),
    x1 = x_center - 0.75 / 4,
    x2 = x_center + 0.75 / 4,
    
    # 图框上缘为 1.24，括号放在框外
    y = 1.34,
    y_tip = 1.25,
    star_y = 1.26
  )

print(sig_df)

# ============================================================
# 3. 绘图
# ============================================================

p_box <- ggplot(
  plot_data,
  aes(x = flow_group, y = r, fill = period_group)
) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.6,
    color = "grey45"
  ) +
  geom_boxplot(
    position = position_dodge(width = 0.75),
    width = 0.58,
    outlier.shape = NA,
    linewidth = 0.7
  ) +
  
  # 显著性括号横线
  geom_segment(
    data = sig_df,
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.8,
    color = "black"
  ) +
  
  # 左侧竖线
  geom_segment(
    data = sig_df,
    aes(x = x1, xend = x1, y = y_tip, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.8,
    color = "black"
  ) +
  
  # 右侧竖线
  geom_segment(
    data = sig_df,
    aes(x = x2, xend = x2, y = y_tip, yend = y),
    inherit.aes = FALSE,
    linewidth = 0.8,
    color = "black"
  ) +
  
  # 星号
  geom_text(
    data = sig_df,
    aes(x = x_center, y = star_y, label = sig_label),
    inherit.aes = FALSE,
    size = 7,
    fontface = "bold",
    color = "black",
    vjust = 0
  ) +
  
  scale_fill_manual(
    values = c(
      "Daytime" = "#fdae61",
      "Nighttime" = "#6BA3D6"
    )
  ) +
  scale_y_continuous(
    breaks = seq(-1, 1, by = 0.5),
    expand = expansion(mult = c(0.12, 0.12))
  ) +
  
  # 在这里控制范围，避免框外显著性标注被删除
  coord_cartesian(
    ylim = c(-1, 1),
    clip = "off"
  ) +
  
  labs(
    x = NULL,
    y = "DO–WT correlation",
    fill = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 20),
    
    # 给图例与框外显著性括号留出距离
    legend.box.spacing = grid::unit(1.2, "cm"),
    
    axis.text.x = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.y = element_text(size = 20),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    ),
    axis.line = element_blank(),
    plot.margin = margin(t = 15, r = 10, b = 10, l = 10)
  )

print(p_box)

ggsave(
  filename = file.path(out_dir, "daynight_flow_significance.png"),
  plot = p_box,
  width = 6,
  height = 4.5,
  dpi = 600,
  bg = "white"
)