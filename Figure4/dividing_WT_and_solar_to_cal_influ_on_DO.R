# ============================================================
# DO-WT mechanism decomposition
# DO = DOsat + DO_excess
# Aim:
#   Separate solubility-driven DO change from non-solubility oxygen gain
# ============================================================

rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

# ============================================================
# 1. Paths
# ============================================================

input_dir <- "D:/Python/R/6 DO-WT/revised_Figure4/拆分WT和solar"

out_dir <- "D:/Python/R/6 DO-WT/revised_Figure4/拆分WT和solar/DO_WT_decomposition_output"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 每天至少多少小时数据才保留
min_day_completeness <- 0.75

# 每天最低记录数，避免过稀疏
min_abs_records_per_day <- 4

# 每个站点或每个昼夜组计算斜率时至少多少条有效记录
min_obs_slope <- 30

# 太阳辐射判断白天/夜间的阈值
solar_day_threshold <- 0.05

# ============================================================
# 2. Helper functions
# ============================================================

read_csv_safe <- function(path) {
  
  out <- tryCatch(
    read_csv(
      path,
      show_col_types = FALSE,
      guess_max = 100000,
      locale = locale(encoding = "UTF-8")
    ),
    error = function(e) {
      read_csv(
        path,
        show_col_types = FALSE,
        guess_max = 100000,
        locale = locale(encoding = "GB18030")
      )
    }
  )
  
  out
}

get_first_col <- function(nms, candidates) {
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

parse_time_safely <- function(x) {
  
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  
  x <- as.character(x)
  
  out <- suppressWarnings(ymd_hms(x, tz = "UTC"))
  
  if (all(is.na(out))) {
    out <- suppressWarnings(
      parse_date_time(
        x,
        orders = c(
          "ymd HMS",
          "ymd HM",
          "ymd HMS z",
          "ymd HM z",
          "ymd_HMS",
          "ymd_HM",
          "ymd",
          "mdy HMS",
          "mdy HM"
        ),
        tz = "UTC"
      )
    )
  }
  
  as.POSIXct(out, tz = "UTC")
}

calc_slope <- function(x, y, min_n = min_obs_slope) {
  
  ok <- is.finite(x) & is.finite(y)
  
  if (sum(ok) < min_n) {
    return(NA_real_)
  }
  
  x_use <- x[ok]
  y_use <- y[ok]
  
  if (sd(x_use, na.rm = TRUE) == 0 || sd(y_use, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  
  unname(coef(lm(y_use ~ x_use))[2])
}

calc_cor <- function(x, y, min_n = min_obs_slope) {
  
  ok <- is.finite(x) & is.finite(y)
  
  if (sum(ok) < min_n) {
    return(NA_real_)
  }
  
  x_use <- x[ok]
  y_use <- y[ok]
  
  if (sd(x_use, na.rm = TRUE) == 0 || sd(y_use, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  
  cor(x_use, y_use, method = "pearson")
}

calc_p <- function(x, y, min_n = min_obs_slope) {
  
  ok <- is.finite(x) & is.finite(y)
  
  if (sum(ok) < min_n) {
    return(NA_real_)
  }
  
  x_use <- x[ok]
  y_use <- y[ok]
  
  if (sd(x_use, na.rm = TRUE) == 0 || sd(y_use, na.rm = TRUE) == 0) {
    return(NA_real_)
  }
  
  cor.test(x_use, y_use, method = "pearson")$p.value
}

calc_decomp_group <- function(.x, .y) {
  
  beta_obs <- calc_slope(.x$WT_anom, .x$DO_anom)
  beta_sat <- calc_slope(.x$WT_anom, .x$DOsat_anom)
  beta_exc <- calc_slope(.x$WT_anom, .x$DO_excess_anom)
  beta_pct <- calc_slope(.x$WT_anom, .x$DO_percent_anom)
  
  r_obs <- calc_cor(.x$WT_anom, .x$DO_anom)
  r_sat <- calc_cor(.x$WT_anom, .x$DOsat_anom)
  r_exc <- calc_cor(.x$WT_anom, .x$DO_excess_anom)
  r_pct <- calc_cor(.x$WT_anom, .x$DO_percent_anom)
  
  p_obs <- calc_p(.x$WT_anom, .x$DO_anom)
  p_sat <- calc_p(.x$WT_anom, .x$DOsat_anom)
  p_exc <- calc_p(.x$WT_anom, .x$DO_excess_anom)
  p_pct <- calc_p(.x$WT_anom, .x$DO_percent_anom)
  
  gamma_obs <- calc_slope(.x$solar_anom, .x$DO_anom)
  gamma_sat <- calc_slope(.x$solar_anom, .x$DOsat_anom)
  gamma_exc <- calc_slope(.x$solar_anom, .x$DO_excess_anom)
  gamma_pct <- calc_slope(.x$solar_anom, .x$DO_percent_anom)
  
  tibble(
    n_obs = nrow(.x),
    n_days = n_distinct(.x$date),
    
    median_dt_hours = median(.x$median_dt_hours, na.rm = TRUE),
    expected_records_per_day = median(.x$expected_records_per_day, na.rm = TRUE),
    min_records_per_day = median(.x$min_records_per_day, na.rm = TRUE),
    
    beta_obs = beta_obs,
    beta_sat = beta_sat,
    beta_exc = beta_exc,
    beta_pct = beta_pct,
    
    r_obs = r_obs,
    r_sat = r_sat,
    r_exc = r_exc,
    r_pct = r_pct,
    
    p_obs = p_obs,
    p_sat = p_sat,
    p_exc = p_exc,
    p_pct = p_pct,
    
    beta_sum_check = beta_sat + beta_exc,
    beta_diff = beta_obs - (beta_sat + beta_exc),
    
    overcomp_ratio = ifelse(
      is.finite(beta_sat) & abs(beta_sat) > 1e-8,
      beta_exc / abs(beta_sat),
      NA_real_
    ),
    
    overcomp_strength = beta_exc - abs(beta_sat),
    
    overcompensated = ifelse(
      is.finite(beta_obs) & is.finite(beta_sat) & is.finite(beta_exc),
      beta_obs > 0 & beta_sat < 0 & beta_exc > abs(beta_sat),
      NA
    ),
    
    gamma_obs = gamma_obs,
    gamma_sat = gamma_sat,
    gamma_exc = gamma_exc,
    gamma_pct = gamma_pct,
    
    gamma_overcomp_ratio = ifelse(
      is.finite(gamma_sat) & abs(gamma_sat) > 1e-8,
      gamma_exc / abs(gamma_sat),
      NA_real_
    ),
    
    solar_mean = mean(.x$solar, na.rm = TRUE),
    DO_mean = mean(.x$DO, na.rm = TRUE),
    WT_mean = mean(.x$WT, na.rm = TRUE),
    DOsat_mean = mean(.x$DOsat, na.rm = TRUE),
    DO_excess_mean = mean(.x$DO_excess, na.rm = TRUE),
    DO_percent_mean = mean(.x$DO_percent, na.rm = TRUE)
  )
}

# ============================================================
# 3. Read all station CSV files
# ============================================================

file_list <- list.files(
  input_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

if (length(file_list) == 0) {
  stop("input_dir 中没有找到 CSV 文件。")
}

read_one_station <- function(file_path) {
  
  raw <- read_csv_safe(file_path)
  
  nms <- names(raw)
  file_name <- tools::file_path_sans_ext(basename(file_path))
  
  time_col <- get_first_col(
    nms,
    c("timestamp", "datetime", "time", "date_time", "DateTime")
  )
  
  do_col <- get_first_col(
    nms,
    c("DO", "avg_DO", "do", "DO_obs", "dissolved_oxygen")
  )
  
  wt_col <- get_first_col(
    nms,
    c("WT", "wt", "water_temperature", "temp", "temperature")
  )
  
  sat_col <- get_first_col(
    nms,
    c("DOsat", "DO_sat", "DOsat_conc", "DO_sat_conc")
  )
  
  pct_col <- get_first_col(
    nms,
    c("DO_percent", "DO_pctsat", "DO_percent_sat", "DOsat_percent")
  )
  
  solar_col <- get_first_col(
    nms,
    c(
      "ALLSKY_SFC_SW_DWN",
      "ALLSKY_SFC_SW_DWN_x",
      "ALLSKY_SFC_SW_DWN_y",
      "solar",
      "Solar",
      "solar_radiation",
      "srad"
    )
  )
  
  if (is.na(time_col) || is.na(do_col) || is.na(wt_col) || is.na(sat_col)) {
    message("Skipped due to missing required columns: ", file_name)
    return(NULL)
  }
  
  DO_vec <- suppressWarnings(as.numeric(raw[[do_col]]))
  WT_vec <- suppressWarnings(as.numeric(raw[[wt_col]]))
  DOsat_vec <- suppressWarnings(as.numeric(raw[[sat_col]]))
  
  if (!is.na(pct_col)) {
    DO_percent_vec <- suppressWarnings(as.numeric(raw[[pct_col]]))
  } else {
    DO_percent_vec <- DO_vec / DOsat_vec * 100
  }
  
  if (!is.na(solar_col)) {
    solar_vec <- suppressWarnings(as.numeric(raw[[solar_col]]))
  } else {
    solar_vec <- rep(NA_real_, nrow(raw))
  }
  
  tibble(
    file_name = file_name,
    datetime = parse_time_safely(raw[[time_col]]),
    DO = DO_vec,
    WT = WT_vec,
    DOsat = DOsat_vec,
    DO_percent = DO_percent_vec,
    solar = solar_vec
  )
}

df_all <- map_dfr(file_list, read_one_station)

if (nrow(df_all) == 0) {
  stop("没有成功读取任何站点数据。请检查列名是否包含 timestamp, DO, WT, DOsat。")
}

# ============================================================
# 4. Basic cleaning
# ============================================================

df_all <- df_all %>%
  mutate(
    date = as.Date(datetime),
    hour = hour(datetime),
    DO_excess = DO - DOsat
  ) %>%
  filter(
    !is.na(datetime),
    is.finite(DO),
    is.finite(WT),
    is.finite(DOsat),
    is.finite(DO_percent),
    DO >= 0,
    DO <= 30,
    WT >= -2,
    WT <= 45,
    DOsat > 0,
    DOsat <= 25,
    DO_percent >= 0,
    DO_percent <= 300
  )

cat("\nTotal observations after basic cleaning:", nrow(df_all), "\n")
cat("Number of sites after basic cleaning:", n_distinct(df_all$file_name), "\n")

write_csv(
  df_all,
  file.path(out_dir, "all_hourly_data_with_DO_excess.csv")
)

# ============================================================
# 5. Adaptive daily high-frequency anomalies
#    Key fix for mixed hourly and 4-hourly data
# ============================================================

# ------------------------------------------------------------
# 5.1 Estimate sampling interval for each station
# ------------------------------------------------------------

sampling_info <- df_all %>%
  arrange(file_name, datetime) %>%
  group_by(file_name) %>%
  mutate(
    dt_hours = as.numeric(difftime(datetime, lag(datetime), units = "hours"))
  ) %>%
  summarise(
    median_dt_hours = median(
      dt_hours[is.finite(dt_hours) & dt_hours > 0 & dt_hours <= 24],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    median_dt_hours = ifelse(is.finite(median_dt_hours), median_dt_hours, 1),
    
    expected_records_per_day = round(24 / median_dt_hours),
    expected_records_per_day = pmin(pmax(expected_records_per_day, 1), 24),
    
    min_records_per_day = ceiling(expected_records_per_day * min_day_completeness),
    min_records_per_day = pmax(min_records_per_day, min_abs_records_per_day),
    min_records_per_day = pmin(min_records_per_day, expected_records_per_day)
  )

write_csv(
  sampling_info,
  file.path(out_dir, "station_sampling_interval_and_daily_threshold.csv")
)

cat("\nSampling interval summary:\n")
print(
  sampling_info %>%
    count(expected_records_per_day, min_records_per_day) %>%
    arrange(expected_records_per_day)
)

# ------------------------------------------------------------
# 5.2 Join sampling information
# ------------------------------------------------------------

df_all2 <- df_all %>%
  left_join(sampling_info, by = "file_name")

# ------------------------------------------------------------
# 5.3 Daily high-frequency anomalies
# ------------------------------------------------------------

df_anom <- df_all2 %>%
  group_by(file_name, date) %>%
  mutate(
    n_records_day = n()
  ) %>%
  filter(
    n_records_day >= first(min_records_per_day)
  ) %>%
  mutate(
    DO_anom = DO - mean(DO, na.rm = TRUE),
    WT_anom = WT - mean(WT, na.rm = TRUE),
    DOsat_anom = DOsat - mean(DOsat, na.rm = TRUE),
    DO_excess_anom = DO_excess - mean(DO_excess, na.rm = TRUE),
    DO_percent_anom = DO_percent - mean(DO_percent, na.rm = TRUE),
    solar_anom = solar - mean(solar, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    period = case_when(
      is.finite(solar) & solar > solar_day_threshold ~ "Daytime",
      is.finite(solar) & solar <= solar_day_threshold ~ "Nighttime",
      hour >= 12 & hour <= 17 ~ "Daytime",
      hour >= 0  & hour <= 5  ~ "Nighttime",
      TRUE ~ "Other"
    )
  )

write_csv(
  df_anom,
  file.path(out_dir, "hourly_daily_anomalies_for_decomposition.csv")
)

cat("\nTotal observations after adaptive daily completeness filtering:", nrow(df_anom), "\n")
cat("Number of sites after adaptive daily completeness filtering:", n_distinct(df_anom$file_name), "\n")

cat("\nSite counts by expected records per day:\n")
print(
  df_anom %>%
    distinct(file_name, expected_records_per_day, min_records_per_day) %>%
    count(expected_records_per_day, min_records_per_day) %>%
    arrange(expected_records_per_day)
)

cat("\nPeriod counts:\n")
print(
  df_anom %>%
    count(period)
)

# ============================================================
# 6. Site-level decomposition using all records
# ============================================================

site_decomp_all <- df_anom %>%
  group_by(file_name) %>%
  group_modify(calc_decomp_group) %>%
  ungroup()

write_csv(
  site_decomp_all,
  file.path(out_dir, "site_DO_WT_decomposition_all_hours.csv")
)

cat("\nAll-hour decomposition sites:", nrow(site_decomp_all), "\n")
cat("Valid beta_obs:", sum(is.finite(site_decomp_all$beta_obs)), "\n")
cat("Valid beta_sat:", sum(is.finite(site_decomp_all$beta_sat)), "\n")
cat("Valid beta_exc:", sum(is.finite(site_decomp_all$beta_exc)), "\n")
cat("Valid gamma_exc:", sum(is.finite(site_decomp_all$gamma_exc)), "\n")

# ============================================================
# 7. Site-level decomposition by daytime and nighttime
# ============================================================

site_decomp_period <- df_anom %>%
  filter(period %in% c("Daytime", "Nighttime")) %>%
  group_by(file_name, period) %>%
  group_modify(calc_decomp_group) %>%
  ungroup()

write_csv(
  site_decomp_period,
  file.path(out_dir, "site_DO_WT_decomposition_daytime_nighttime.csv")
)

cat("\nDay/night decomposition rows:", nrow(site_decomp_period), "\n")
cat("Valid day/night beta_obs:", sum(is.finite(site_decomp_period$beta_obs)), "\n")

# ============================================================
# 8. Summary statistics
# ============================================================

summary_all <- site_decomp_all %>%
  summarise(
    n_sites = n(),
    
    median_beta_obs = median(beta_obs, na.rm = TRUE),
    median_beta_sat = median(beta_sat, na.rm = TRUE),
    median_beta_exc = median(beta_exc, na.rm = TRUE),
    
    median_overcomp_ratio = median(overcomp_ratio, na.rm = TRUE),
    mean_overcomp_ratio = mean(overcomp_ratio, na.rm = TRUE),
    
    pct_beta_obs_positive = mean(beta_obs > 0, na.rm = TRUE) * 100,
    pct_beta_sat_negative = mean(beta_sat < 0, na.rm = TRUE) * 100,
    pct_beta_exc_positive = mean(beta_exc > 0, na.rm = TRUE) * 100,
    pct_overcompensated = mean(overcompensated, na.rm = TRUE) * 100,
    
    median_r_obs = median(r_obs, na.rm = TRUE),
    median_r_sat = median(r_sat, na.rm = TRUE),
    median_r_exc = median(r_exc, na.rm = TRUE),
    median_r_pct = median(r_pct, na.rm = TRUE),
    
    median_gamma_obs = median(gamma_obs, na.rm = TRUE),
    median_gamma_sat = median(gamma_sat, na.rm = TRUE),
    median_gamma_exc = median(gamma_exc, na.rm = TRUE)
  )

summary_period <- site_decomp_period %>%
  group_by(period) %>%
  summarise(
    n_sites = n(),
    
    median_beta_obs = median(beta_obs, na.rm = TRUE),
    median_beta_sat = median(beta_sat, na.rm = TRUE),
    median_beta_exc = median(beta_exc, na.rm = TRUE),
    
    median_overcomp_ratio = median(overcomp_ratio, na.rm = TRUE),
    mean_overcomp_ratio = mean(overcomp_ratio, na.rm = TRUE),
    
    pct_beta_obs_positive = mean(beta_obs > 0, na.rm = TRUE) * 100,
    pct_beta_sat_negative = mean(beta_sat < 0, na.rm = TRUE) * 100,
    pct_beta_exc_positive = mean(beta_exc > 0, na.rm = TRUE) * 100,
    pct_overcompensated = mean(overcompensated, na.rm = TRUE) * 100,
    
    median_r_obs = median(r_obs, na.rm = TRUE),
    median_r_sat = median(r_sat, na.rm = TRUE),
    median_r_exc = median(r_exc, na.rm = TRUE),
    median_r_pct = median(r_pct, na.rm = TRUE),
    
    median_gamma_obs = median(gamma_obs, na.rm = TRUE),
    median_gamma_sat = median(gamma_sat, na.rm = TRUE),
    median_gamma_exc = median(gamma_exc, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(summary_all, file.path(out_dir, "summary_decomposition_all_hours.csv"))
write_csv(summary_period, file.path(out_dir, "summary_decomposition_daytime_nighttime.csv"))

cat("\nSummary all hours:\n")
print(summary_all)

cat("\nSummary by period:\n")
print(summary_period)

# ============================================================
# 9. Plot 1: slope decomposition for all records
# ============================================================

slope_long_all <- site_decomp_all %>%
  select(file_name, beta_obs, beta_sat, beta_exc) %>%
  pivot_longer(
    cols = c(beta_obs, beta_sat, beta_exc),
    names_to = "component",
    values_to = "slope"
  ) %>%
  mutate(
    component = factor(
      component,
      levels = c("beta_sat", "beta_exc", "beta_obs"),
      labels = c("Solubility component", "Non-solubility component", "Observed DO")
    )
  )

p_slope_all <- ggplot(slope_long_all, aes(x = component, y = slope)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) +
  geom_boxplot(outlier.shape = NA, width = 0.55) +
  geom_jitter(width = 0.15, size = 0.8, alpha = 0.25) +
  theme_classic(base_size = 14) +
  labs(
    x = NULL,
    y = expression("Slope with WT anomaly (mg L"^-1*" "*degree*C^-1*")")
  ) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank()
  )

print(p_slope_all)

ggsave(
  file.path(out_dir, "Fig_slope_decomposition_all_hours.png"),
  p_slope_all,
  width = 6.2,
  height = 4.5,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(out_dir, "Fig_slope_decomposition_all_hours.pdf"),
  p_slope_all,
  width = 6.2,
  height = 4.5,
  bg = "white"
)

# ============================================================
# 10. Plot 2: daytime vs nighttime decomposition
# ============================================================

slope_long_period <- site_decomp_period %>%
  select(file_name, period, beta_obs, beta_sat, beta_exc) %>%
  pivot_longer(
    cols = c(beta_obs, beta_sat, beta_exc),
    names_to = "component",
    values_to = "slope"
  ) %>%
  mutate(
    component = factor(
      component,
      levels = c("beta_sat", "beta_exc", "beta_obs"),
      labels = c("Solubility", "Non-solubility", "Observed")
    ),
    period = factor(period, levels = c("Daytime", "Nighttime"))
  )

p_slope_period <- ggplot(slope_long_period, aes(x = component, y = slope)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.6) +
  geom_boxplot(outlier.shape = NA, width = 0.55) +
  geom_jitter(width = 0.15, size = 0.7, alpha = 0.22) +
  facet_wrap(~ period, nrow = 1) +
  theme_classic(base_size = 14) +
  labs(
    x = NULL,
    y = expression("Slope with WT anomaly (mg L"^-1*" "*degree*C^-1*")")
  ) +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank()
  )

print(p_slope_period)

ggsave(
  file.path(out_dir, "Fig_slope_decomposition_daytime_nighttime.png"),
  p_slope_period,
  width = 7.2,
  height = 4.5,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(out_dir, "Fig_slope_decomposition_daytime_nighttime.pdf"),
  p_slope_period,
  width = 7.2,
  height = 4.5,
  bg = "white"
)

# ============================================================
# 11. Plot 3: overcompensation ratio
# ============================================================

p_overcomp <- site_decomp_all %>%
  filter(
    is.finite(overcomp_ratio),
    overcomp_ratio >= -5,
    overcomp_ratio <= 10
  ) %>%
  ggplot(aes(x = overcomp_ratio)) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.8) +
  geom_histogram(bins = 40, color = "white") +
  theme_classic(base_size = 14) +
  labs(
    x = expression("Overcompensation ratio: " * beta[excess] / abs(beta[sat])),
    y = "Number of sites"
  ) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
    axis.line = element_blank()
  )

print(p_overcomp)

ggsave(
  file.path(out_dir, "Fig_overcompensation_ratio_histogram.png"),
  p_overcomp,
  width = 5.5,
  height = 4.3,
  dpi = 600,
  bg = "white"
)

ggsave(
  file.path(out_dir, "Fig_overcompensation_ratio_histogram.pdf"),
  p_overcomp,
  width = 5.5,
  height = 4.3,
  bg = "white"
)

# ============================================================
# 12. Plot 4: solar versus overcompensation ratio
# ============================================================

if (sum(is.finite(site_decomp_all$solar_mean)) > 10) {
  
  p_solar_overcomp <- site_decomp_all %>%
    filter(
      is.finite(solar_mean),
      is.finite(overcomp_ratio),
      overcomp_ratio >= -5,
      overcomp_ratio <= 10
    ) %>%
    ggplot(aes(x = solar_mean, y = overcomp_ratio)) +
    geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.7) +
    geom_point(size = 1.5, alpha = 0.45) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1.1) +
    theme_classic(base_size = 14) +
    labs(
      x = "Mean solar radiation",
      y = expression("Overcompensation ratio: " * beta[excess] / abs(beta[sat]))
    ) +
    theme(
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      axis.line = element_blank()
    )
  
  print(p_solar_overcomp)
  
  ggsave(
    file.path(out_dir, "Fig_solar_vs_overcompensation_ratio.png"),
    p_solar_overcomp,
    width = 5.5,
    height = 4.3,
    dpi = 600,
    bg = "white"
  )
  
  ggsave(
    file.path(out_dir, "Fig_solar_vs_overcompensation_ratio.pdf"),
    p_solar_overcomp,
    width = 5.5,
    height = 4.3,
    bg = "white"
  )
}

message("\nAdaptive DO-WT decomposition finished. Outputs saved to: ", out_dir)