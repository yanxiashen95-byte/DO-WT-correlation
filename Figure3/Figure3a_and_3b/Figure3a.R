library(tidyverse)
library(sf)

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
df <- read_csv("day_night.csv", show_col_types = FALSE)

df2 <- df %>%
  mutate(
    r_day   = as.numeric(r_day),
    r_night = as.numeric(r_night),
    Lat     = as.numeric(Lat),
    Lon     = as.numeric(Lon)
  ) %>%
  filter(!is.na(r_day), !is.na(r_night), !is.na(Lat), !is.na(Lon)) %>%
  mutate(
    delta = r_day - r_night  # ✅ Day - Night
  )

# 为了让颜色条对称，取 delta 绝对值的最大值作为 scale 边界
lim <- max(abs(df2$delta), na.rm = TRUE)

# ===============================
# 3. 作图
# ===============================
p_map <- ggplot() +
  geom_sf(data = world, fill = "grey95", color = "grey70", linewidth = 0.2) +
  
  geom_sf(
    data = st_as_sf(df2, coords = c("Lon", "Lat"), crs = 4326, remove = FALSE),
    aes(fill = delta),
    shape  = 21,
    size   = 2.5,       
    color  = "white",   
    stroke = 0.3,
    alpha  = 0.9
  ) +
  
  # === 关键修改：锁定范围并在边缘处“挤压”颜色 ===
  scale_fill_gradient2(
    low      = "#2C7BB6", 
    mid      = "white",   
    high     = "#f46d43", 
    midpoint = 0,
    limits   = c(-0.2, 0.2),           # ✅ 调节为你想要的范围
    oob      = scales::squish,         # ✅ 核心：让超出 0.5 的点显示为最深红色
    name     = expression(Delta * "r (Day - Night)")
  ) +
  
  coord_sf(xlim = c(-180, 180), ylim = c(-60, 90), expand = FALSE) +
  scale_x_continuous(breaks = seq(-180, 180, by = 60)) +
  scale_y_continuous(breaks = seq(-60, 90, by = 30)) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid      = element_blank(),
    panel.border    = element_rect(color = "black", fill = NA, linewidth = 0.8),
    legend.position = "right",
    axis.text       = element_text(size = 11)
  )

print(p_map)

ggsave("map_delta_amp.png", p_map, width = 8, height = 4.8, dpi = 300)
