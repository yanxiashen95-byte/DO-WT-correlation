library(sf)
library(dplyr)
library(readr)
library(ggplot2)

# ===============================
# 1. 读取世界地图
# ===============================
world <- st_read(
  "D:/Python/R/0. paper_plog/hourly_and_data_DO/点和地图的叠加/世界地图.shp", 
  quiet = TRUE
) %>%
  st_transform(4326)

# ===============================
# 2. 读取并清洗数据
# ===============================
df <- read_csv("lag5_WT_lon_lat.csv", show_col_types = FALSE)

df_do <- df %>%
  mutate(
    lon = as.numeric(lon),
    lat = as.numeric(lat),
    site_median_r = as.numeric(site_median_r)
  ) %>%
  filter(
    variable == "WT",
    !is.na(lon),
    !is.na(lat),
    !is.na(site_median_r)
  )

# ===============================
# 3. 绘图
# ===============================
p <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey75", linewidth = 0.25) +
  geom_point(
    data = df_do,
    aes(x = lon, y = lat, fill = site_median_r),
    shape = 21,
    size = 6,
    alpha = 0.9,
    color = "white",
    stroke = 0.7
  ) +
  scale_fill_gradient(
    low = "#f7f7f7",
    high = "#ef8a62",
    limits = c(0, 1),
    name = expression(italic(r))
  ) +
  coord_sf(
    xlim = c(-180, 180),
    ylim = c(-60, 80),
    expand = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(-60, 90, by = 30)
  ) +
  labs(
    x = NULL,
    y = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.text = element_text(size = 18, color = "black"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16)
  )

print(p)

# ===============================
# 4. 保存图片
# ===============================
ggsave(
  "WT_site_median_r_world_map.png",
  p,
  width = 11,
  height = 5.8,
  dpi = 300
)
