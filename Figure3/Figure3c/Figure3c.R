
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
  scale_fill_manual(
    values = c(
      "Daytime" = "#fdae61",
      "Nighttime" = "#6BA3D6"
    )
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.5)
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
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    axis.line = element_blank()
  )

print(p_box)

ggsave(
  filename = file.path(out_dir, "boxplot_raw_daily_daynight_r_by_flow.png"),
  plot = p_box,
  width = 6,
  height = 4.5,
  dpi = 600
)

ggsave(
  filename = file.path(out_dir, "boxplot_raw_daily_daynight_r_by_flow.pdf"),
  plot = p_box,
  width = 6.5,
  height = 4.5
)
