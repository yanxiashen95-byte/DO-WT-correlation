
library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(tidyr)

input_dir <- "D:/Python/R/6 DO-WT/revised_Figure4/solar和DO_WT关系"

out_dir <- file.path(input_dir, "station_line_plot")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

min_days_per_site <- 5

files <- list.files(
  input_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

files <- files[
  !grepl(
    "summary|global|site_level|station_line|solar_group|plot",
    basename(files),
    ignore.case = TRUE
  )
]

read_one_site <- function(file_path) {
  
  site_id_from_file <- tools::file_path_sans_ext(basename(file_path)) %>%
    as.character()
  
  df <- read_csv(file_path, show_col_types = FALSE)
  names(df) <- str_trim(names(df))
  
  required_cols <- c(
    "date",
    "solar_daylight_mean",
    "r_DO_WT_daylight"
  )
  
  if (!all(required_cols %in% names(df))) {
    message("跳过文件，缺少必要列：", basename(file_path))
    return(tibble())
  }
  
  df %>%
    mutate(
      date = as.character(date),
      solar_daylight_mean = as.numeric(solar_daylight_mean),
      r_DO_WT_daylight = as.numeric(r_DO_WT_daylight)
    ) %>%
    filter(
      is.finite(solar_daylight_mean),
      is.finite(r_DO_WT_daylight),
      r_DO_WT_daylight >= -1,
      r_DO_WT_daylight <= 1
    ) %>%
    group_by(date) %>%
    summarise(
      solar_daylight_mean = median(solar_daylight_mean, na.rm = TRUE),
      r_DO_WT_daylight = median(r_DO_WT_daylight, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    transmute(
      site_id = as.character(site_id_from_file),
      date = date,
      solar_daylight_mean = solar_daylight_mean,
      r_DO_WT_daylight = r_DO_WT_daylight
    )
}

all_df <- map_dfr(files, read_one_site)

cat("读取后的总记录数：", nrow(all_df), "\n")
cat("读取后的总站点数：", n_distinct(all_df$site_id), "\n")

valid_sites <- all_df %>%
  group_by(site_id) %>%
  summarise(
    n_days = n(),
    sd_solar = sd(solar_daylight_mean, na.rm = TRUE),
    sd_r = sd(r_DO_WT_daylight, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(
    n_days >= min_days_per_site,
    is.finite(sd_solar),
    sd_solar > 0,
    is.finite(sd_r),
    sd_r > 0
  )

all_df2 <- all_df %>%
  filter(site_id %in% valid_sites$site_id)

cat("筛选后的总记录数：", nrow(all_df2), "\n")
cat("筛选后的总站点数：", n_distinct(all_df2$site_id), "\n")

write_csv(
  all_df2,
  file.path(out_dir, "all_site_daily_DO_WT_r_and_solar_clean.csv")
)

fit_one_site <- function(df_site) {
  
  sid <- unique(df_site$site_id)[1]
  
  if (
    nrow(df_site) < min_days_per_site ||
    sd(df_site$solar_daylight_mean, na.rm = TRUE) == 0 ||
    sd(df_site$r_DO_WT_daylight, na.rm = TRUE) == 0
  ) {
    return(tibble())
  }
  
  fit <- lm(r_DO_WT_daylight ~ solar_daylight_mean, data = df_site)
  
  intercept <- unname(coef(fit)[1])
  slope <- unname(coef(fit)[2])
  
  x_seq <- seq(
    min(df_site$solar_daylight_mean, na.rm = TRUE),
    max(df_site$solar_daylight_mean, na.rm = TRUE),
    length.out = 50
  )
  
  tibble(
    site_id = as.character(sid),
    solar_daylight_mean = x_seq,
    fitted_r = intercept + slope * x_seq
  )
}

station_line_df <- all_df2 %>%
  group_by(site_id) %>%
  group_split() %>%
  map_dfr(fit_one_site)

global_fit <- lm(r_DO_WT_daylight ~ solar_daylight_mean, data = all_df2)

x_global <- seq(
  min(all_df2$solar_daylight_mean, na.rm = TRUE),
  max(all_df2$solar_daylight_mean, na.rm = TRUE),
  length.out = 300
)

global_line_df <- tibble(
  solar_daylight_mean = x_global,
  fitted_r = unname(coef(global_fit)[1]) +
    unname(coef(global_fit)[2]) * x_global
)

cat("全球拟合截距：", unname(coef(global_fit)[1]), "\n")
cat("全球拟合斜率：", unname(coef(global_fit)[2]), "\n")

site_coef_df <- all_df2 %>%
  group_by(site_id) %>%
  group_modify(~ {
    
    dat <- .x
    
    if (
      nrow(dat) < min_days_per_site ||
      sd(dat$solar_daylight_mean, na.rm = TRUE) == 0 ||
      sd(dat$r_DO_WT_daylight, na.rm = TRUE) == 0
    ) {
      return(
        tibble(
          n_days = nrow(dat),
          intercept = NA_real_,
          slope = NA_real_,
          r_site = NA_real_,
          p_site = NA_real_
        )
      )
    }
    
    fit <- lm(r_DO_WT_daylight ~ solar_daylight_mean, data = dat)
    ct <- cor.test(
      dat$solar_daylight_mean,
      dat$r_DO_WT_daylight,
      method = "pearson"
    )
    
    tibble(
      n_days = nrow(dat),
      intercept = unname(coef(fit)[1]),
      slope = unname(coef(fit)[2]),
      r_site = unname(ct$estimate),
      p_site = ct$p.value
    )
  }) %>%
  ungroup()

write_csv(
  site_coef_df,
  file.path(out_dir, "site_level_solar_DO_WT_line_stats.csv")
)

slope_summary <- site_coef_df %>%
  filter(is.finite(slope)) %>%
  summarise(
    n_sites = n(),
    positive_slope_sites = sum(slope > 0),
    positive_slope_percent = positive_slope_sites / n_sites * 100,
    median_slope = median(slope, na.rm = TRUE),
    mean_slope = mean(slope, na.rm = TRUE),
    median_r_site = median(r_site, na.rm = TRUE)
  )

print(slope_summary)

write_csv(
  slope_summary,
  file.path(out_dir, "global_slope_summary.csv")
)


p <- ggplot() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "grey45",
    linewidth = 0.6
  ) +
  geom_line(
    data = station_line_df,
    aes(
      x = solar_daylight_mean,
      y = fitted_r,
      group = site_id
    ),
    color = "grey75",
    alpha = 0.25,
    linewidth = 0.28
  ) +
  geom_line(
    data = global_line_df,
    aes(
      x = solar_daylight_mean,
      y = fitted_r
    ),
    color = "red",
    linewidth = 2
  ) +
  coord_cartesian(
    ylim = c(-1.05, 1.25)
  ) +
  labs(
    x = "Solar radiation (kWh m⁻² day⁻¹)",
    y = "DO–WT correlation (r)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(size = 20, color = "black"),
    axis.title = element_text(size = 20, color = "black"),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1
    ),
    axis.line = element_blank(),
    plot.margin = margin(10, 15, 10, 10)
  )

print(p)

ggsave(
  filename = file.path(out_dir, "all_station_lines_global_red_line.png"),
  plot = p,
  width = 8,
  height = 3.5,
  dpi = 600
)

p_slope <- site_coef_df %>%
  filter(is.finite(slope)) %>%
  ggplot(aes(x = slope)) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey45",
    linewidth = 0.6
  ) +
  geom_histogram(
    bins = 50,
    fill = "grey65",
    color = "white",
    linewidth = 0.2
  ) +
  labs(
    x = "Site-level slope",
    y = "Number of sites"
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(size = 14, color = "black"),
    axis.title = element_text(size = 16, color = "black"),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    axis.line = element_blank()
  )

print(p_slope)

ggsave(
  filename = file.path(out_dir, "site_level_slope_histogram.png"),
  plot = p_slope,
  width = 6,
  height = 4.5,
  dpi = 600
)

cat("全部完成。结果已保存到：\n", out_dir, "\n")