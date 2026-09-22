library(readr)
library(dplyr)
library(purrr)
library(stringr)

input_dir <- "D:/Python/R/6 DO-WT/Figure2/Figure2/r_Q_merged"
output_dir <- file.path(input_dir, "flow_season_r_slope_summary")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

flow_seasons <- c("low_flow", "normal_flow", "high_flow")

read_one_station <- function(file_path) {
  
  df <- read_csv(file_path, show_col_types = FALSE)
  
  station_id_from_file <- tools::file_path_sans_ext(basename(file_path))
  
  if (!"station_id" %in% names(df)) {
    df <- df %>%
      mutate(station_id = station_id_from_file)
  }
  
  if (!all(c("station_id", "r", "slope", "flow_season") %in% names(df))) {
    warning(paste0("文件缺少必要列，已跳过：", basename(file_path)))
    return(tibble())
  }
  
  df %>%
    mutate(
      station_id = as.character(station_id),
      r = as.numeric(r),
      slope = as.numeric(slope),
      flow_season = as.character(flow_season),
      source_file = basename(file_path)
    ) %>%
    filter(flow_season %in% flow_seasons)
}

csv_files <- list.files(
  input_dir,
  pattern = "\\.csv$",
  full.names = TRUE,
  recursive = FALSE
)

cat("识别到文件数量：", length(csv_files), "\n")

all_data <- map_dfr(csv_files, read_one_station)

summary_df <- all_data %>%
  group_by(station_id, flow_season) %>%
  summarise(
    n_days = n(),
    n_valid_r = sum(is.finite(r)),
    n_valid_slope = sum(is.finite(slope)),
    r_mean = mean(r, na.rm = TRUE),
    r_median = median(r, na.rm = TRUE),
    slope_mean = mean(slope, na.rm = TRUE),
    slope_median = median(slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    r_mean = ifelse(is.nan(r_mean), NA_real_, r_mean),
    r_median = ifelse(is.nan(r_median), NA_real_, r_median),
    slope_mean = ifelse(is.nan(slope_mean), NA_real_, slope_mean),
    slope_median = ifelse(is.nan(slope_median), NA_real_, slope_median)
  ) %>%
  arrange(station_id, flow_season)

write_csv(
  summary_df,
  file.path(output_dir, "all_sites_flow_season_r_slope_summary.csv")
)

for (season in flow_seasons) {
  
  season_df <- summary_df %>%
    filter(flow_season == season) %>%
    arrange(station_id)
  
  write_csv(
    season_df,
    file.path(output_dir, paste0("all_sites_", season, "_r_slope_summary.csv"))
  )
  
  cat("完成：", season, "，站点数量：", nrow(season_df), "\n")
}

cat("全部完成！\n")
cat("输出文件夹：", output_dir, "\n")