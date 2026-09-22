library(readr)
library(dplyr)
library(lubridate)
library(purrr)
library(stringr)

input_dir <- "D:/Python/R/6 DO-WT/revised_Figure3/DO"

output_dir <- "D:/Python/R/6 DO-WT/revised_Figure3/Japan"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

min_points <- 5

parse_time_safe <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  
  parse_date_time(
    x,
    orders = c(
      "ymd HMS",
      "ymd HM",
      "ymd H",
      "ymd",
      "Ymd HMS",
      "Ymd HM",
      "Ymd H",
      "Ymd"
    ),
    tz = "UTC",
    quiet = TRUE
  )
}

calc_daily_r_one_site <- function(file_path) {
  
  site_id <- tools::file_path_sans_ext(basename(file_path))
  
  df <- read_csv(file_path, show_col_types = FALSE)
  names(df) <- str_trim(names(df))
  
  if (!all(c("timestamp", "avg_DO", "wt") %in% names(df))) {
    message("⚠ Skipped, missing columns: ", site_id)
    return(NULL)
  }
  
  df_check <- df %>%
    mutate(
      timestamp_chr = as.character(timestamp),
      parsed_time = parse_time_safe(timestamp_chr),
      date = as_date(parsed_time),
      hour = hour(parsed_time)
    )
  
  message(
    "站点 ", site_id,
    " 原始0点记录数：",
    sum(str_detect(as.character(df$timestamp), "00:00:00"), na.rm = TRUE),
    "；解析后0点记录数：",
    sum(df_check$hour == 0, na.rm = TRUE)
  )
  
  df_daynight <- df_check %>%
    mutate(
      avg_DO = as.numeric(avg_DO),
      wt = as.numeric(wt),
      period = case_when(
        hour %in% 12:17 ~ "daytime",
        hour %in% 0:5 ~ "nighttime",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(
      !is.na(avg_DO),
      !is.na(wt),
      !is.na(date),
      !is.na(hour),
      !is.na(period)
    )
  
  message(
    "站点 ", site_id,
    " 进入昼夜分析的0点记录数：",
    sum(df_daynight$hour == 0, na.rm = TRUE)
  )
  
  if (nrow(df_daynight) == 0) {
    message("⚠ No valid day/night data for site: ", site_id)
    return(NULL)
  }
  
  daily_r <- df_daynight %>%
    group_by(date, period) %>%
    summarise(
      n = n(),
      r = ifelse(
        n() >= min_points &&
          sd(wt, na.rm = TRUE) > 0 &&
          sd(avg_DO, na.rm = TRUE) > 0,
        cor(wt, avg_DO, use = "complete.obs"),
        NA_real_
      ),
      p = ifelse(
        n() >= 3 &&
          sd(wt, na.rm = TRUE) > 0 &&
          sd(avg_DO, na.rm = TRUE) > 0,
        tryCatch(cor.test(wt, avg_DO)$p.value, error = function(e) NA_real_),
        NA_real_
      ),
      slope = ifelse(
        n() >= min_points &&
          sd(wt, na.rm = TRUE) > 0,
        tryCatch(coef(lm(avg_DO ~ wt))[2], error = function(e) NA_real_),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    filter(n >= min_points)
  
  out_file <- file.path(
    output_dir,
    paste0(site_id, "_daily_r.csv")
  )
  
  write_csv(daily_r, out_file)
  
  message("✔ Finished site: ", site_id)
}

file_list <- list.files(
  input_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

walk(file_list, calc_daily_r_one_site)