library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)
library(tibble)

input_dir <- "D:/Python/R/6 DO-WT/revised_Figure4/China"

output_dir <- "D:/Python/R/6 DO-WT/revised_Figure4/中国/DO_WT_solar_daylight"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

min_points <- 3
solar_threshold <- 0

parse_time_safe <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_replace_all(x, "T", " ")
  x <- str_remove(x, "Z$")
  
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

calc_cor <- function(x, y) {
  
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  valid <- is.finite(x) & is.finite(y)
  x <- x[valid]
  y <- y[valid]
  
  n_valid <- length(x)
  
  if (
    n_valid < min_points ||
    sd(x, na.rm = TRUE) == 0 ||
    sd(y, na.rm = TRUE) == 0
  ) {
    return(
      tibble(
        n = n_valid,
        r = NA_real_,
        p = NA_real_
      )
    )
  }
  
  ct <- tryCatch(
    cor.test(x, y, method = "pearson"),
    error = function(e) NULL
  )
  
  tibble(
    n = n_valid,
    r = ifelse(is.null(ct), NA_real_, unname(ct$estimate)),
    p = ifelse(is.null(ct), NA_real_, ct$p.value)
  )
}

calc_one_site <- function(file_path) {
  
  site_id <- tools::file_path_sans_ext(basename(file_path)) %>%
    str_remove("^merged_")
  
  out_file <- file.path(
    output_dir,
    paste0(site_id, "_daily_daylight_DO_WT_r_and_solar.csv")
  )
  
  if (file.exists(out_file)) {
    message("已存在，跳过站点：", site_id)
    return(NULL)
  }
  
  df <- read_csv(file_path, show_col_types = FALSE)
  names(df) <- str_trim(names(df))
  
  required_cols <- c(
    "timestamp",
    "DO",
    "WT",
    "ALLSKY_SFC_SW_DWN"
  )
  
  if (!all(required_cols %in% names(df))) {
    message("跳过，缺少必要列：", site_id)
    return(NULL)
  }
  
  df2 <- df %>%
    mutate(
      DO = as.numeric(DO),
      WT = as.numeric(WT),
      ALLSKY_SFC_SW_DWN = as.numeric(ALLSKY_SFC_SW_DWN),
      parsed_time = parse_time_safe(timestamp),
      date = as_date(parsed_time),
      hour = hour(parsed_time)
    ) %>%
    filter(
      !is.na(parsed_time),
      !is.na(date),
      is.finite(DO),
      is.finite(WT),
      is.finite(ALLSKY_SFC_SW_DWN)
    ) %>%
    arrange(parsed_time)
  
  if (nrow(df2) == 0) {
    message("跳过，无有效数据：", site_id)
    return(NULL)
  }
  
  daily_result <- df2 %>%
    group_by(date) %>%
    group_modify(~ {
      
      daylight_df <- .x %>%
        filter(ALLSKY_SFC_SW_DWN > solar_threshold)
      
      cor_daylight <- calc_cor(daylight_df$DO, daylight_df$WT) %>%
        rename(
          n_DO_WT_daylight = n,
          r_DO_WT_daylight = r,
          p_DO_WT_daylight = p
        )
      
      tibble(
        n_hour_daylight = nrow(daylight_df),
        solar_daylight_mean = ifelse(
          nrow(daylight_df) > 0,
          mean(daylight_df$ALLSKY_SFC_SW_DWN, na.rm = TRUE),
          NA_real_
        ),
        solar_daylight_sum = ifelse(
          nrow(daylight_df) > 0,
          sum(daylight_df$ALLSKY_SFC_SW_DWN, na.rm = TRUE),
          NA_real_
        ),
        solar_daylight_max = ifelse(
          nrow(daylight_df) > 0,
          max(daylight_df$ALLSKY_SFC_SW_DWN, na.rm = TRUE),
          NA_real_
        )
      ) %>%
        bind_cols(cor_daylight)
      
    }) %>%
    ungroup() %>%
    mutate(
      site_id = site_id,
      .before = 1
    )
  
  write_csv(daily_result, out_file)
  
  message(
    "完成并保存站点：", site_id,
    "，days = ", nrow(daily_result),
    "，valid r_DO_WT_daylight = ", sum(is.finite(daily_result$r_DO_WT_daylight)),
    "，保存到：", out_file
  )
  
  rm(df, df2, daily_result)
  gc()
  
  return(NULL)
}

files <- list.files(
  input_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

walk(files, calc_one_site)

cat("全部完成。每个站点的结果已单独保存到：\n", output_dir, "\n")