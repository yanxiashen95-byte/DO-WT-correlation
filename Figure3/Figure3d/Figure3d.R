library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(lubridate)
library(ggplot2)
library(tibble)

input_dir <- "D:/Python/R/6 DO-WT/revised_Figure3/global"

coord_file <- "D:/Python/R/6 DO-WT/revised_Figure3/lon_and_lat.csv"

out_dir <- "D:/Python/R/6 DO-WT/revised_Figure3/climate_zone_boxplot"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

parse_date_safe <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  
  parse_date_time(
    x,
    orders = c(
      "ymd", "Ymd",
      "ymd HMS", "ymd HM",
      "Ymd HMS", "Ymd HM",
      "mdy", "dmy"
    ),
    tz = "UTC",
    quiet = TRUE
  ) %>%
    as_date()
}

classify_climate_zone <- function(lat) {
  abs_lat <- abs(as.numeric(lat))
  
  case_when(
    abs_lat < 23.5 ~ "Tropical",
    abs_lat >= 23.5 & abs_lat < 66.5 ~ "Temperate",
    abs_lat >= 66.5 ~ "Cold",
    TRUE ~ NA_character_
  )
}

coords <- read_csv(coord_file, show_col_types = FALSE)
names(coords) <- str_trim(names(coords))

coords <- coords %>%
  rename(
    station_id = Station,
    lon = longitude,
    lat = latitude
  ) %>%
  mutate(
    station_id = as.character(station_id),
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    climate_zone = classify_climate_zone(lat)
  ) %>%
  filter(
    !is.na(station_id),
    is.finite(lon),
    is.finite(lat),
    !is.na(climate_zone)
  ) %>%
  distinct(station_id, .keep_all = TRUE)

read_one_daynight_file <- function(file_path) {
  
  file_name <- basename(file_path)
  station_id_from_file <- tools::file_path_sans_ext(file_name)
  
  df <- read_csv(file_path, show_col_types = FALSE)
  names(df) <- str_trim(names(df))
  
  if (!all(c("date", "period", "n", "r", "p", "slope") %in% names(df))) {
    warning(paste0("文件缺少必要列，已跳过：", file_name))
    return(tibble())
  }
  
  df %>%
    mutate(
      station_id = as.character(station_id_from_file),
      date = parse_date_safe(date),
      period = as.character(period),
      n = as.numeric(n),
      r = as.numeric(r),
      p = as.numeric(p),
      slope = as.numeric(slope),
      source_file = file_name
    ) %>%
    filter(
      !is.na(date),
      period %in% c("daytime", "nighttime"),
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

all_r_data <- map_dfr(csv_files, read_one_daynight_file)

plot_data <- all_r_data %>%
  left_join(
    coords %>% select(station_id, lon, lat, climate_zone),
    by = "station_id"
  ) %>%
  filter(!is.na(climate_zone)) %>%
  mutate(
    period_group = recode(
      period,
      "daytime" = "Daytime",
      "nighttime" = "Nighttime"
    ),
    climate_zone = factor(
      climate_zone,
      levels = c("Tropical", "Temperate", "Cold")
    ),
    period_group = factor(
      period_group,
      levels = c("Daytime", "Nighttime")
    )
  )

write_csv(
  plot_data,
  file.path(out_dir, "daily_daynight_r_with_climate_zone.csv")
)

unmatched_sites <- all_r_data %>%
  distinct(station_id) %>%
  anti_join(coords %>% distinct(station_id), by = "station_id")

write_csv(
  unmatched_sites,
  file.path(out_dir, "unmatched_sites_without_coordinates.csv")
)

summary_table <- plot_data %>%
  group_by(climate_zone, period_group) %>%
  summarise(
    n_records = n(),
    n_sites = n_distinct(station_id),
    mean_r = mean(r, na.rm = TRUE),
    median_r = median(r, na.rm = TRUE),
    q25 = quantile(r, 0.25, na.rm = TRUE),
    q75 = quantile(r, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(
  summary_table,
  file.path(out_dir, "summary_daynight_r_by_climate_zone.csv")
)

print(summary_table)

p_box <- ggplot(
  plot_data,
  aes(x = climate_zone, y = r, fill = period_group)
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
    legend.text = element_text(size = 21),
    axis.text.x = element_text(size = 21),
    axis.text.y = element_text(size = 21),
    axis.title.y = element_text(size = 21),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    axis.line = element_blank()
  )

print(p_box)

ggsave(
  filename = file.path(out_dir, "boxplot_daily_daynight_r_by_climate_zone.png"),
  plot = p_box,
  width = 6,
  height = 4.5,
  dpi = 600
)

ggsave(
  filename = file.path(out_dir, "boxplot_daily_daynight_r_by_climate_zone.pdf"),
  plot = p_box,
  width = 6.5,
  height = 4.5
)