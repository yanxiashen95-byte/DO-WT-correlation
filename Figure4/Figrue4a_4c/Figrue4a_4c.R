# ============================================================
# Separate global maps and separate density plots
#
# Main maps:
#   a. beta_sat   = solubility loss       -> WHITE point border
#   b. beta_exc   = non-solubility gain   -> BLACK point border
#   c. gamma_exc  = solar-linked gain     -> BLACK point border
# ============================================================

rm(list = ls())
gc()

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(scales)
})

# ============================================================
# 1. Paths
# ============================================================

decomp_path <- "site_DO_WT_decomposition_all_hours.csv"

coord_path <- "lon_and_lat.csv"

world_shp <-
  "D:/Python/R/0. paper_plog/hourly_and_data_DO/点和地图的叠加/世界地图.shp"

out_dir <-
  "DO_WT_decomposition_output/separate_maps_and_density"

dir.create(
  out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

show_axes <- TRUE

map_xlim <- c(-180, 180)
map_ylim <- c(-60, 85)


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
  
  if (length(hit) == 0) {
    return(NA_character_)
  }
  
  hit[1]
}


clean_id <- function(x) {
  
  x <- as.character(x)
  
  x <- iconv(
    x,
    from = "",
    to = "UTF-8",
    sub = ""
  )
  
  x <- gsub("\\\\", "/", x)
  x <- sub(".*/", "", x)
  x <- sub("\\.[Cc][Ss][Vv]$", "", x)
  x <- sub("^merged_", "", x, ignore.case = TRUE)
  x <- sub("^X+", "", x)
  x <- sub("_+$", "", x)
  x <- trimws(x)
  
  x
}


get_symmetric_limit <- function(x, q = 0.98) {
  
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(c(-1, 1))
  }
  
  lim <- as.numeric(
    quantile(
      abs(x),
      probs = q,
      na.rm = TRUE
    )
  )
  
  if (!is.finite(lim) || lim == 0) {
    lim <- max(abs(x), na.rm = TRUE)
  }
  
  c(-lim, lim)
}


lon_lab <- function(x) {
  
  x <- round(x)
  
  ifelse(
    x < 0,
    paste0(abs(x), "°W"),
    ifelse(
      x > 0,
      paste0(x, "°E"),
      "0°"
    )
  )
}


lat_lab <- function(x) {
  
  x <- round(x)
  
  ifelse(
    x < 0,
    paste0(abs(x), "°S"),
    ifelse(
      x > 0,
      paste0(x, "°N"),
      "0°"
    )
  )
}


map_theme <- function(show_axes = TRUE) {
  
  if (show_axes) {
    
    theme_classic(base_size = 13) +
      theme(
        axis.text = element_text(
          color = "black",
          size = 11
        ),
        
        axis.title = element_blank(),
        
        axis.ticks = element_line(
          color = "black",
          linewidth = 0.4
        ),
        
        axis.line = element_blank(),
        
        panel.border = element_rect(
          color = "black",
          fill = NA,
          linewidth = 0.8
        ),
        
        panel.background = element_rect(
          fill = "white",
          color = NA
        ),
        
        plot.background = element_rect(
          fill = "white",
          color = NA
        ),
        
        plot.title = element_text(
          size = 15,
          face = "bold",
          hjust = 0
        ),
        
        legend.title = element_text(
          size = 15,
          color = "black"
        ),
        
        legend.text = element_text(
          size = 15,
          color = "black"
        ),
        
        legend.position = c(0.08, 0.24),
        
        plot.margin = margin(5, 5, 5, 5)
      )
    
  } else {
    
    theme_void(base_size = 13) +
      theme(
        
        panel.border = element_rect(
          color = "black",
          fill = NA,
          linewidth = 0.8
        ),
        
        panel.background = element_rect(
          fill = "white",
          color = NA
        ),
        
        plot.background = element_rect(
          fill = "white",
          color = NA
        ),
        
        plot.title = element_text(
          size = 15,
          face = "bold",
          hjust = 0
        ),
        
        legend.title = element_text(
          size = 15,
          color = "black"
        ),
        
        legend.text = element_text(
          size = 15,
          color = "black"
        ),
        
        legend.position = c(0.08, 0.24),
        
        plot.margin = margin(5, 5, 5, 5)
      )
  }
}


# ============================================================
# 3. Read decomposition results
# ============================================================

decomp <- read_csv_safe(decomp_path)

if (!"file_name" %in% names(decomp)) {
  stop(
    "site_DO_WT_decomposition_all_hours.csv 中没有 file_name 列。"
  )
}


need_cols <- c(
  "beta_sat",
  "beta_exc",
  "gamma_exc"
)

miss_cols <- setdiff(
  need_cols,
  names(decomp)
)


if (length(miss_cols) > 0) {
  
  stop(
    paste0(
      "分解结果中缺少这些列: ",
      paste(
        miss_cols,
        collapse = ", "
      )
    )
  )
}


decomp <- decomp %>%
  mutate(
    
    site_key = clean_id(file_name),
    
    beta_sat = as.numeric(beta_sat),
    beta_exc = as.numeric(beta_exc),
    gamma_exc = as.numeric(gamma_exc),
    
    beta_obs =
      if ("beta_obs" %in% names(.))
        as.numeric(beta_obs)
    else
      NA_real_,
    
    gamma_obs =
      if ("gamma_obs" %in% names(.))
        as.numeric(gamma_obs)
    else
      NA_real_,
    
    overcomp_ratio =
      if ("overcomp_ratio" %in% names(.))
        as.numeric(overcomp_ratio)
    else
      NA_real_
  )


cat(
  "\nDecomposition rows:",
  nrow(decomp),
  "\n"
)

cat(
  "Valid beta_sat:",
  sum(is.finite(decomp$beta_sat)),
  "\n"
)

cat(
  "Valid beta_exc:",
  sum(is.finite(decomp$beta_exc)),
  "\n"
)

cat(
  "Valid gamma_exc:",
  sum(is.finite(decomp$gamma_exc)),
  "\n"
)


# ============================================================
# 4. Read coordinates
# ============================================================

coord_raw <- read_csv_safe(coord_path)

nms <- names(coord_raw)


id_col <- get_first_col(
  nms,
  c(
    "file_name",
    "site_key",
    "site_id",
    "siteID",
    "site_clean",
    "gauge_id",
    "id",
    "ID"
  )
)


lon_col <- get_first_col(
  nms,
  c(
    "lon",
    "longitude",
    "Longitude",
    "LONGITUDE",
    "x",
    "X"
  )
)


lat_col <- get_first_col(
  nms,
  c(
    "lat",
    "latitude",
    "Latitude",
    "LATITUDE",
    "y",
    "Y"
  )
)


if (
  is.na(id_col) ||
  is.na(lon_col) ||
  is.na(lat_col)
) {
  
  stop(
    "经纬度表需要包含站点 ID、lon 和 lat。请检查 coord_path。"
  )
}


coord <- coord_raw %>%
  transmute(
    
    site_key =
      clean_id(.data[[id_col]]),
    
    lon =
      as.numeric(.data[[lon_col]]),
    
    lat =
      as.numeric(.data[[lat_col]])
  ) %>%
  
  filter(
    is.finite(lon),
    is.finite(lat),
    lon >= -180,
    lon <= 180,
    lat >= -90,
    lat <= 90
  ) %>%
  
  distinct(
    site_key,
    .keep_all = TRUE
  )


cat(
  "\nCoordinate rows:",
  nrow(coord),
  "\n"
)


# ============================================================
# 5. Merge decomposition and coordinates
# ============================================================

map_data <- decomp %>%
  
  left_join(
    coord,
    by = "site_key"
  ) %>%
  
  filter(
    is.finite(lon),
    is.finite(lat)
  )


cat(
  "\nNumber of sites with coordinates:",
  nrow(map_data),
  "\n"
)

cat(
  "Mapped valid beta_sat:",
  sum(is.finite(map_data$beta_sat)),
  "\n"
)

cat(
  "Mapped valid beta_exc:",
  sum(is.finite(map_data$beta_exc)),
  "\n"
)

cat(
  "Mapped valid gamma_exc:",
  sum(is.finite(map_data$gamma_exc)),
  "\n"
)


write_csv(
  map_data,
  file.path(
    out_dir,
    "site_decomposition_with_coordinates.csv"
  )
)


if (nrow(map_data) == 0) {
  stop(
    "没有站点成功匹配经纬度，无法画图。"
  )
}


# ============================================================
# 6. Read world map
# ============================================================

world <- st_read(
  world_shp,
  quiet = TRUE
)


if (is.na(st_crs(world))) {
  st_crs(world) <- 4326
}


world_4326 <- st_transform(
  world,
  crs = 4326
)


points_sf <- st_as_sf(
  map_data,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)


# ============================================================
# 7. Map function
#
# 新增：
# point_border_color
#
# a = white
# b/c = black
# ============================================================

make_separate_map <- function(
    points_sf,
    world_4326,
    var,
    title_text,
    legend_title,
    file_prefix,
    lims = NULL,
    point_border_color = "white"
) {
  
  data_df <- points_sf %>%
    
    st_drop_geometry() %>%
    
    filter(
      is.finite(.data[[var]])
    )
  
  
  cat(
    "\n",
    title_text,
    "valid points:",
    nrow(data_df),
    "\n"
  )
  
  
  if (nrow(data_df) == 0) {
    
    warning(
      paste0(
        "No valid points for ",
        var
      )
    )
    
    return(NULL)
  }
  
  
  if (is.null(lims)) {
    
    lims <- get_symmetric_limit(
      data_df[[var]],
      q = 0.98
    )
  }
  
  
  p <- ggplot() +
    
    # 世界地图
    geom_sf(
      data = world_4326,
      fill = "grey96",
      color = "grey75",
      linewidth = 0.25
    ) +
    
    # 站点
    geom_sf(
      data =
        points_sf %>%
        filter(
          is.finite(.data[[var]])
        ),
      
      aes(
        fill = .data[[var]]
      ),
      
      shape = 21,
      
      size = 3.8,
      
      # 点边框粗细
      stroke = 0.55,
      
      # ============================
      # a/b/c 分别由函数参数控制
      # ============================
      color = point_border_color,
      
      alpha = 0.92
    ) +
    
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#fc8d59",
      midpoint = 0,
      limits = lims,
      oob = squish,
      name = legend_title
    ) +
    
    coord_sf(
      xlim = map_xlim,
      ylim = map_ylim,
      expand = FALSE,
      crs = st_crs(4326)
    ) +
    
    scale_x_continuous(
      breaks = seq(
        -120,
        120,
        by = 60
      ),
      labels = lon_lab
    ) +
    
    scale_y_continuous(
      breaks = seq(
        -60,
        60,
        by = 30
      ),
      labels = lat_lab
    ) +
    
    guides(
      fill = guide_colorbar(
        
        title.position = "top",
        title.hjust = 0,
        
        label.position = "right",
        
        barwidth =
          grid::unit(
            4.5,
            "mm"
          ),
        
        barheight =
          grid::unit(
            32,
            "mm"
          ),
        
        ticks = TRUE,
        
        # 图例标题字号
        title.theme =
          element_text(
            size = 15,
            lineheight = 0.9
          ),
        
        # 图例刻度数字字号
        label.theme =
          element_text(
            size = 15
          )
      )
    ) +
    
    labs(
      title = title_text
    ) +
    
    map_theme(
      show_axes = show_axes
    ) +
    
    theme(
      
      axis.text.x =
        element_text(
          color = "black",
          size = 15
        ),
      
      axis.text.y =
        element_text(
          color = "black",
          size = 15
        ),
      
      axis.title =
        element_blank(),
      
      axis.ticks =
        element_line(
          color = "black",
          linewidth = 0.4
        ),
      
      axis.line =
        element_blank(),
      
      panel.border =
        element_rect(
          color = "black",
          fill = NA,
          linewidth = 0.8
        ),
      
      panel.background =
        element_rect(
          fill = "white",
          color = NA
        ),
      
      plot.background =
        element_rect(
          fill = "white",
          color = NA
        ),
      
      plot.title =
        element_text(
          size = 15,
          face = "bold",
          hjust = 0
        ),
      
      legend.title =
        element_text(
          size = 15,
          color = "black"
        ),
      
      legend.text =
        element_text(
          size = 15,
          color = "black"
        ),
      
      legend.position =
        c(0.12, 0.30),
      
      plot.margin =
        margin(5, 5, 5, 5)
    )
  
  
  print(p)
  
  
  ggsave(
    filename =
      file.path(
        out_dir,
        paste0(
          file_prefix,
          ".png"
        )
      ),
    
    plot = p,
    
    width = 10,
    height = 4.6,
    dpi = 600,
    
    bg = "white"
  )
  
  
  return(p)
}


# ============================================================
# 8. Density function
# ============================================================

make_separate_density <- function(
    data,
    var,
    xlab_text,
    file_prefix,
    lims = NULL
) {
  
  data_df <- data %>%
    filter(
      is.finite(.data[[var]])
    )
  
  
  if (nrow(data_df) == 0) {
    
    warning(
      paste0(
        "No valid data for density: ",
        var
      )
    )
    
    return(NULL)
  }
  
  
  if (is.null(lims)) {
    
    lims <- get_symmetric_limit(
      data_df[[var]],
      q = 0.98
    )
  }
  
  
  p <- ggplot(
    data_df,
    aes(
      x = .data[[var]]
    )
  ) +
    
    geom_density(
      fill = "grey70",
      color = "grey25",
      linewidth = 0.6,
      alpha = 0.85
    ) +
    
    geom_vline(
      xintercept = 0,
      linetype = "dashed",
      linewidth = 0.7
    ) +
    
    coord_cartesian(
      xlim = lims
    ) +
    
    theme_classic(
      base_size = 13
    ) +
    
    labs(
      x = xlab_text,
      y = "Density"
    ) +
    
    theme(
      
      axis.text.x =
        element_text(
          color = "black",
          size = 20
        ),
      
      axis.text.y =
        element_text(
          color = "black",
          size = 20
        ),
      
      axis.title.x =
        element_text(
          color = "black",
          size = 20
        ),
      
      axis.title.y =
        element_text(
          color = "black",
          size = 20
        ),
      
      panel.border =
        element_rect(
          color = "black",
          fill = NA,
          linewidth = 0.8
        ),
      
      axis.line =
        element_blank(),
      
      plot.background =
        element_rect(
          fill = "white",
          color = NA
        ),
      
      panel.background =
        element_rect(
          fill = "white",
          color = NA
        ),
      
      plot.margin =
        margin(
          5,
          5,
          5,
          5
        )
    )
  
  
  print(p)
  
  
  ggsave(
    filename =
      file.path(
        out_dir,
        paste0(
          file_prefix,
          ".png"
        )
      ),
    
    plot = p,
    
    width = 4.2,
    height = 3.2,
    dpi = 600,
    
    bg = "white"
  )
  
  
  return(p)
}


# ============================================================
# 9. Set color limits
# ============================================================

lim_beta_sat <-
  get_symmetric_limit(
    map_data$beta_sat,
    q = 0.98
  )

lim_beta_exc <-
  get_symmetric_limit(
    map_data$beta_exc,
    q = 0.98
  )

lim_gamma_exc <-
  get_symmetric_limit(
    map_data$gamma_exc,
    q = 0.98
  )


limits_table <- tibble(
  
  variable =
    c(
      "beta_sat",
      "beta_exc",
      "gamma_exc"
    ),
  
  lower =
    c(
      lim_beta_sat[1],
      lim_beta_exc[1],
      lim_gamma_exc[1]
    ),
  
  upper =
    c(
      lim_beta_sat[2],
      lim_beta_exc[2],
      lim_gamma_exc[2]
    )
)


write_csv(
  limits_table,
  file.path(
    out_dir,
    "map_color_limits.csv"
  )
)


print(limits_table)


# ============================================================
# 10. Separate maps
# ============================================================


# ------------------------------------------------------------
# a. Solubility-related DO change
# WHITE border
# ------------------------------------------------------------

p_map_a <- make_separate_map(
  
  points_sf = points_sf,
  
  world_4326 = world_4326,
  
  var = "beta_sat",
  
  title_text =
    "Solubility-related DO change",
  
  legend_title =
    "Solubility-related\nDO change\n(mg L⁻¹ °C⁻¹)",
  
  file_prefix =
    "Map_a_solubility_related_DO_change_beta_sat",
  
  lims =
    lim_beta_sat,
  
  # ============================
  # a 图白色边框
  # ============================
  point_border_color = "white"
)



# ------------------------------------------------------------
# b. Non-solubility-related DO change
# BLACK border
# ------------------------------------------------------------

p_map_b <- make_separate_map(
  
  points_sf = points_sf,
  
  world_4326 = world_4326,
  
  var = "beta_exc",
  
  title_text =
    "Non-solubility-related DO change",
  
  legend_title =
    "Non-solubility-related\nDO change\n(mg L⁻¹ °C⁻¹)",
  
  file_prefix =
    "Map_b_non_solubility_related_DO_change_beta_excess",
  
  lims =
    lim_beta_exc,
  
  # ============================
  # b 图黑色边框
  # ============================
  point_border_color = "grey70"
)



# ------------------------------------------------------------
# c. Solar-related DO excess change
# BLACK border
# ------------------------------------------------------------

p_map_c <- make_separate_map(
  
  points_sf = points_sf,
  
  world_4326 = world_4326,
  
  var = "gamma_exc",
  
  title_text =
    "Solar-related DO excess change",
  
  legend_title =
    "Solar-related\nDO excess change\n(mg L⁻¹ per solar unit)",
  
  file_prefix =
    "Map_c_solar_related_DO_excess_change_gamma_excess",
  
  lims =
    lim_gamma_exc,
  
  # ============================
  # c 图黑色边框
  # ============================
  point_border_color = "grey70"
)


# ============================================================
# 11. Separate density plots
# ============================================================

p_den_a <- make_separate_density(
  
  data = map_data,
  
  var = "beta_sat",
  
  xlab_text =
    "Solubility-related DO change\n(mg L⁻¹ °C⁻¹)",
  
  file_prefix =
    "Density_a_solubility_related_DO_change_beta_sat",
  
  lims =
    lim_beta_sat
)


p_den_b <- make_separate_density(
  
  data = map_data,
  
  var = "beta_exc",
  
  xlab_text =
    "WT-associated DO excess change\n(mg L⁻¹ °C⁻¹)",
  
  file_prefix =
    "Density_b_non_solubility_related_DO_change_beta_excess",
  
  lims =
    lim_beta_exc
)


p_den_c <- make_separate_density(
  
  data = map_data,
  
  var = "gamma_exc",
  
  xlab_text =
    "Solar-related DO excess change\n(mg L⁻¹ per solar unit)",
  
  file_prefix =
    "Density_c_solar_related_DO_excess_change_gamma_excess",
  
  lims =
    lim_gamma_exc
)


message(
  "\nFinished. Separate maps and density plots saved to: ",
  out_dir
)