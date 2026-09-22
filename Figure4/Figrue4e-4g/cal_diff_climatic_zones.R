# ============================================================
# Plot three decomposition parameters across climate zones
# Climate zones are classified by absolute latitude:
#   Tropical:  abs_lat < 23.5
#   Temperate: 23.5 <= abs_lat < 66.5
#   Cold:      abs_lat >= 66.5
# ============================================================

rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
})

# ============================================================
# 1. Paths
# ============================================================

out_dir <- "D:/Python/R/6 DO-WT/revised_Figure4/拆分WT和solar/DO_WT_decomposition_output"

decomp_path <- file.path(out_dir, "site_DO_WT_decomposition_all_hours.csv")

# 站点经纬度文件
# 需要包含 file_name / site_id / ID 之类的站点列，以及 lon/lat 或 longitude/latitude
coord_path <- "D:/Python/R/6 DO-WT/revised_Figure4/拆分WT和solar/DO_WT_decomposition_output/lon_and_lat.csv"

fig_dir <- file.path(out_dir, "plot_three_parameters_by_climate_zone")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# 2. Helper functions
# ============================================================

read_csv_safe <- function(path) {
  tryCatch(
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
}

get_first_col <- function(nms, candidates) {
  hit <- candidates[candidates %in% nms]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

clean_id <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "UTF-8", sub = "")
  x <- gsub("\\\\", "/", x)
  x <- sub(".*/", "", x)
  x <- sub("\\.[Cc][Ss][Vv]$", "", x)
  x <- sub("^merged_", "", x, ignore.case = TRUE)
  x <- sub("^X+", "", x)
  x <- sub("_+$", "", x)
  x <- trimws(x)
  x
}

theme_pub <- function(base_size = 14) {
  theme_classic(base_size = base_size) +
    theme(
      axis.text = element_text(color = "black", size = 18),
      axis.title = element_text(color = "black", size = 19),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      axis.line = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(size = 18, face = "bold", hjust = 0),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# ============================================================
# 3. Read decomposition data
# ============================================================

site_decomp_all <- read_csv_safe(decomp_path) %>%
  mutate(
    site_key = clean_id(file_name),
    beta_sat = as.numeric(beta_sat),
    beta_exc = as.numeric(beta_exc),
    gamma_exc = as.numeric(gamma_exc)
  )

cat("\nDecomposition rows:", nrow(site_decomp_all), "\n")
cat("Valid beta_sat:", sum(is.finite(site_decomp_all$beta_sat)), "\n")
cat("Valid beta_exc:", sum(is.finite(site_decomp_all$beta_exc)), "\n")
cat("Valid gamma_exc:", sum(is.finite(site_decomp_all$gamma_exc)), "\n")

# ============================================================
# 4. Read coordinate data
# ============================================================

coord_raw <- read_csv_safe(coord_path)

nms <- names(coord_raw)

id_col <- get_first_col(
  nms,
  c("file_name", "site_key", "site_id", "siteID", "site_clean", "gauge_id", "id", "ID")
)

lon_col <- get_first_col(
  nms,
  c("lon", "longitude", "Longitude", "LONGITUDE", "x", "X")
)

lat_col <- get_first_col(
  nms,
  c("lat", "latitude", "Latitude", "LATITUDE", "y", "Y")
)

cat("\nDetected ID column:", id_col, "\n")
cat("Detected lon column:", lon_col, "\n")
cat("Detected lat column:", lat_col, "\n")

if (is.na(id_col) || is.na(lat_col)) {
  stop("经纬度表需要包含站点 ID 和 lat/latitude 列。")
}

coord <- coord_raw %>%
  transmute(
    site_key = clean_id(.data[[id_col]]),
    lon = if (!is.na(lon_col)) as.numeric(.data[[lon_col]]) else NA_real_,
    lat = as.numeric(.data[[lat_col]])
  ) %>%
  filter(
    !is.na(site_key),
    is.finite(lat),
    lat >= -90,
    lat <= 90
  ) %>%
  distinct(site_key, .keep_all = TRUE)

cat("Coordinate rows:", nrow(coord), "\n")

# ============================================================
# 5. Merge and classify climate zones
# ============================================================

plot_data_all <- site_decomp_all %>%
  left_join(coord, by = "site_key") %>%
  filter(is.finite(lat)) %>%
  mutate(
    abs_lat = abs(lat),
    climate_zone = case_when(
      abs_lat < 23.5 ~ "Tropical",
      abs_lat >= 23.5 & abs_lat < 66.5 ~ "Temperate",
      abs_lat >= 66.5 ~ "Cold",
      TRUE ~ NA_character_
    ),
    climate_zone = factor(
      climate_zone,
      levels = c("Tropical", "Temperate", "Cold")
    )
  ) %>%
  filter(!is.na(climate_zone))

cat("\nMatched sites with latitude:", nrow(plot_data_all), "\n")
print(plot_data_all %>% count(climate_zone))

write_csv(
  plot_data_all,
  file.path(fig_dir, "site_decomposition_with_climate_zone.csv")
)

# ============================================================
# 6. Colors and manual y-axis limits
#    You can adjust these values directly
# ============================================================

climate_cols <- c(
  "Tropical" = "#F28E2B",
  "Temperate" = "#31A354",
  "Cold" = "#2C7FB8"
)

# 手动调整 y 轴范围
ylim_beta_sat <- c(-0.3, -0.1)
breaks_beta_sat <- seq(-0.30, -0.1, by = 0.05)

ylim_beta_exc <- c(-0.4, 2)
breaks_beta_exc <- seq(0, 2, by = 0.5)

ylim_gamma_exc <- c(-0.5, 1)
breaks_gamma_exc <- seq(-0.5, 1.0, by = 0.5)

# 字体大小
x_text_size <- 18
y_text_size <- 18
y_title_size <- 19

# ============================================================
# 7. Common plotting function
# ============================================================

make_climate_boxplot <- function(
    data,
    var,
    ylab_text,
    file_prefix,
    ylim_manual = NULL,
    y_breaks = NULL
) {
  
  plot_data <- data %>%
    filter(
      !is.na(climate_zone),
      is.finite(.data[[var]])
    )
  
  if (is.null(ylim_manual)) {
    y_lim <- quantile(
      plot_data[[var]],
      probs = c(0.01, 0.99),
      na.rm = TRUE
    )
  } else {
    y_lim <- ylim_manual
  }
  
  p <- ggplot(
    plot_data,
    aes(x = climate_zone, y = .data[[var]], fill = climate_zone)
  ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    geom_boxplot(
      outlier.shape = NA,
      width = 0.58,
      color = "black",
      linewidth = 0.75,
      alpha = 0.85
    ) +
    scale_fill_manual(values = climate_cols, drop = FALSE) +
    coord_cartesian(ylim = y_lim) +
    scale_y_continuous(breaks = y_breaks) +
    scale_x_discrete(
      drop = FALSE,
      expand = expansion(mult = c(0.25, 0.25))
    ) +
    labs(
      x = NULL,
      y = ylab_text
    ) +
    theme_pub(base_size = 14) +
    theme(
      axis.text.x = element_text(
        size = x_text_size,
        color = "black",
        angle = 0,
        hjust = 0.5
      ),
      axis.text.y = element_text(
        size = y_text_size,
        color = "black"
      ),
      axis.title.y = element_text(
        size = y_title_size,
        color = "black"
      ),
      legend.position = "none"
    )
  
  print(p)
  
  ggsave(
    file.path(fig_dir, paste0(file_prefix, ".png")),
    p,
    width = 6,
    height = 3.5,
    dpi = 600,
    bg = "white"
  )
  
  
  return(p)
}

# ============================================================
# 8. Three climate-zone boxplots
# ============================================================

p_beta_sat_climate <- make_climate_boxplot(
  data = plot_data_all,
  var = "beta_sat",
  ylab_text = "Solubility-related DO",
  file_prefix = "Panel_climate_beta_sat",
  ylim_manual = ylim_beta_sat,
  y_breaks = breaks_beta_sat
)

p_beta_exc_climate <- make_climate_boxplot(
  data = plot_data_all,
  var = "beta_exc",
  ylab_text = "Non-solubility-related DO",
  file_prefix = "Panel_climate_beta_excess",
  ylim_manual = ylim_beta_exc,
  y_breaks = breaks_beta_exc
)

p_gamma_exc_climate <- make_climate_boxplot(
  data = plot_data_all,
  var = "gamma_exc",
  ylab_text = "Solar-related DO excess",
  file_prefix = "Panel_climate_gamma_excess",
  ylim_manual = ylim_gamma_exc,
  y_breaks = breaks_gamma_exc
)

# ============================================================
# 9. Combine three panels
# ============================================================

p_three_climate <- plot_grid(
  p_beta_sat_climate,
  p_beta_exc_climate,
  p_gamma_exc_climate,
  labels = c("h", "i", "j"),
  nrow = 1,
  align = "hv",
  label_size = 18,
  label_fontface = "bold"
)

print(p_three_climate)

ggsave(
  file.path(fig_dir, "Combined_three_parameters_by_climate_zone.png"),
  p_three_climate,
  width = 16,
  height = 4.6,
  dpi = 600,
  bg = "white"
)

message("\nFinished. Outputs saved to: ", fig_dir)

#===================== 统计中位数和sd ===============================
#===================== 统计中位数和sd ===============================
#===================== 统计中位数和sd ===============================
# ============================================================
# 5.1 Summary statistics for each box in the three panels
# ============================================================

summary_three_boxes <- plot_data_all %>%
  select(
    climate_zone,
    beta_sat,
    beta_exc,
    gamma_exc
  ) %>%
  pivot_longer(
    cols = c(beta_sat, beta_exc, gamma_exc),
    names_to = "parameter",
    values_to = "value"
  ) %>%
  mutate(
    parameter = recode(
      parameter,
      beta_sat  = "Solubility-related DO",
      beta_exc  = "Non-solubility-related DO",
      gamma_exc = "Solar-related DO excess"
    ),
    parameter = factor(
      parameter,
      levels = c(
        "Solubility-related DO",
        "Non-solubility-related DO",
        "Solar-related DO excess"
      )
    )
  ) %>%
  filter(
    !is.na(climate_zone),
    is.finite(value)
  ) %>%
  group_by(parameter, climate_zone) %>%
  summarise(
    n = n(),
    median = median(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    mean = mean(value, na.rm = TRUE),
    q25 = quantile(value, 0.25, na.rm = TRUE),
    q75 = quantile(value, 0.75, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(parameter, climate_zone)

# 查看结果
print(
  summary_three_boxes %>%
    mutate(
      across(
        c(median, sd, mean, q25, q75, iqr, min, max),
        ~ round(.x, 4)
      )
    ),
  n = Inf
)

# 保存完整统计结果
write_csv(
  summary_three_boxes,
  file.path(fig_dir, "summary_statistics_three_climate_boxplots.csv")
)

# ============================================================
# 5.2 Optional: make a compact median ± sd table
# ============================================================

summary_three_boxes_compact <- summary_three_boxes %>%
  mutate(
    median_sd = sprintf("%.3f ± %.3f", median, sd)
  ) %>%
  select(
    parameter,
    climate_zone,
    n,
    median_sd
  )

print(summary_three_boxes_compact, n = Inf)

write_csv(
  summary_three_boxes_compact,
  file.path(fig_dir, "summary_median_sd_three_climate_boxplots.csv")
)