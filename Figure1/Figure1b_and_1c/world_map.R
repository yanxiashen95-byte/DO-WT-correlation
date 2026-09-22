world <- st_read("D:/Python/R/0. paper_plog/hourly_and_data_DO/点和地图的叠加/世界地图.shp")

df <- read_csv("global_high.csv")  # 修改为你的数据路径


df <- df %>%
  mutate(
    slope_category = case_when(
      slope > 0 ~ "Positive",
      slope < 0 ~ "Negative",
      TRUE ~ "Zero"
    )
  )

# === 转换为 sf 对象并提取坐标 ===
df_sf <- st_as_sf(df, coords = c("lon_wgs84", "lat_wgs84"), crs = 4326)
df_pts <- df_sf %>%
  mutate(
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  ) %>%
  as.data.frame()

# === 设置颜色映射 ===
color_map <- c(
  "Positive" = "#ef8a62",  # 红色
  "Negative" = "#4575b4",  # 蓝色
  "Zero" = "gray80"        # 可选：零斜率灰色
)

# === 绘图 ===
p <- ggplot() +
  geom_sf(data = world, fill = "gray95", color = "gray70") +
  geom_point(
    data = df_pts,
    aes(x = lon, y = lat, fill = slope_category),
    shape = 21,
    size = 7,
    stroke = 0.4,
    color = "white"
  ) +
  scale_fill_manual(values = color_map, name = "DO-WT correlation") +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, by = 60)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 30)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 24),
    legend.position = "right",
    legend.title = element_text(size = 24, face = "bold"),
    legend.text = element_text(size = 22),
    plot.title = element_text(size = 24, face = "bold"),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  labs(
    title = "Global Distribution of DO–WT Slope",
    x = "",
    y = ""
  )

print(p)

ggsave("global_DO_WT_slope_map_high.png", plot = p, width = 16, height = 6.5, dpi = 600)
