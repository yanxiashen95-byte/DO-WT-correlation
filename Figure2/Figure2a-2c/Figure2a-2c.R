library(tidyverse)
library(sf)
library(ggplot2)

world <- st_read(
  "D:/Python/R/0. paper_plog/hourly_and_data_DO/点和地图的叠加/世界地图.shp",
  quiet = TRUE
) %>%
  st_transform(4326)

low_file <- "low_flow.csv"
normal_file <- "normal_flow.csv"
high_file <- "high_flow.csv"

read_flow_summary <- function(file_path, flow_label) {
  
  read_csv(file_path, show_col_types = FALSE) %>%
    mutate(
      station_id = as.character(station_id),
      flow_season = flow_label,
      r_median = as.numeric(r_median),
      lon = as.numeric(lon),
      lat = as.numeric(lat)
    ) %>%
    filter(
      is.finite(lon),
      is.finite(lat),
      is.finite(r_median),
      lon >= -180,
      lon <= 180,
      lat >= -90,
      lat <= 90
    )
}

low_df <- read_flow_summary(low_file, "Low-flow")
normal_df <- read_flow_summary(normal_file, "Normal-flow")
high_df <- read_flow_summary(high_file, "High-flow")

make_single_map <- function(plot_data, title_text) {
  
  ggplot() +
    geom_sf(
      data = world,
      fill = "grey95",
      color = "grey70",
      linewidth = 0.2
    ) +
    geom_point(
      data = plot_data,
      aes(x = lon, y = lat, fill = r_median),
      shape = 21,
      size = 3.6,
      alpha = 0.9,
      color = "grey50",
      stroke = 0.25
    ) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#fc8d59",
      midpoint = 0,
      limits = c(-1, 1),
      breaks = c(-1, -0.5, 0, 0.5, 1),
      name = "Median DO–WT\ncorrelation"
    ) +
    scale_y_continuous(
      breaks = seq(-60, 90, by = 30),
      labels = function(x) ifelse(
        x > 0, paste0(x, "°N"),
        ifelse(x < 0, paste0(abs(x), "°S"), "0°")
      )
    ) +
    coord_sf(
      xlim = c(-180, 180),
      ylim = c(-60, 85),
      expand = TRUE
    ) +
    theme_classic(base_size = 14) +
    theme(
      plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 19),
      axis.text = element_text(size = 18),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 13),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      axis.line = element_blank()
    )
}

p_low <- make_single_map(
  plot_data = low_df,
  title_text = "Low-flow"
)

print(p_low)


p_normal <- make_single_map(
  plot_data = normal_df,
  title_text = "Normal-flow"
)

print(p_normal)

p_high <- make_single_map(
  plot_data = high_df,
  title_text = "High-flow"
)

print(p_high)


out_dir <- "map_output"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

ggsave(
  filename = file.path(out_dir, "map_r_median_low_flow.png"),
  plot = p_low,
  width = 10,
  height = 5.5,
  dpi = 600
)

ggsave(
  filename = file.path(out_dir, "map_r_median_normal_flow.png"),
  plot = p_normal,
  width = 10,
  height = 5.5,
  dpi = 600
)

ggsave(
  filename = file.path(out_dir, "map_r_median_high_flow.png"),
  plot = p_high,
  width = 10,
  height = 5.5,
  dpi = 600
)