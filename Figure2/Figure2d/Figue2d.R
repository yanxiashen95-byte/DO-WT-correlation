# =========================== 画密度图  =================================
# =========================== 画密度图  =================================
# =========================== 画密度图  =================================

library(readr)
library(dplyr)
library(ggplot2)

low_file <- "all_sites_low_flow_r_slope_summary.csv"
normal_file <- "all_sites_normal_flow_r_slope_summary.csv"
high_file <- "all_sites_high_flow_r_slope_summary.csv"

out_file <- "r_median_density.png"

low_df <- read_csv(low_file, show_col_types = FALSE) %>%
  mutate(flow_group = "Low-flow")

normal_df <- read_csv(normal_file, show_col_types = FALSE) %>%
  mutate(flow_group = "Normal-flow")

high_df <- read_csv(high_file, show_col_types = FALSE) %>%
  mutate(flow_group = "High-flow")

plot_df <- bind_rows(low_df, normal_df, high_df) %>%
  mutate(
    r_median = as.numeric(r_median),
    flow_group = factor(
      flow_group,
      levels = c("Low-flow", "Normal-flow", "High-flow")
    )
  ) %>%
  filter(
    !is.na(r_median),
    is.finite(r_median)
  )

median_df <- plot_df %>%
  group_by(flow_group) %>%
  summarise(
    xintercept = median(r_median, na.rm = TRUE),
    .groups = "drop"
  )

p <- ggplot(
  plot_df,
  aes(
    x = r_median,
    color = flow_group,
    linetype = flow_group
  )
) +
  
  # ==========================================================
# Density curves：只画线，不填充
# ==========================================================
geom_density(
  linewidth = 1.8,
  adjust = 1
) +
  
  # ==========================================================
# Median dashed lines
# ==========================================================
geom_vline(
  data = median_df,
  aes(
    xintercept = xintercept,
    color = flow_group
  ),
  linetype = "dashed",
  linewidth = 1.0,
  show.legend = FALSE
) +
  
  # ==========================================================
# Colors
# ==========================================================
scale_color_manual(
  values = c(
    "Low-flow" = "#2C7FB8",
    "Normal-flow" = "#31A354",
    "High-flow" = "#F28E2B"
  )
) +
  
  # ==========================================================
# Linetypes
# ==========================================================
scale_linetype_manual(
  values = c(
    "Low-flow" = "solid",
    "Normal-flow" = "dashed",
    "High-flow" = "dotdash"
  )
) +
  
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, by = 0.5),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.01, 0.08))
  ) +
  
  labs(
    x = "Median DO–WT correlation",
    y = "Normalized density",
    color = NULL,
    linetype = NULL
  ) +
  
  theme_classic(base_size = 14) +
  
  theme(
    legend.position = "bottom",
    
    legend.text = element_text(
      size = 23
    ),
    
    legend.key.width = grid::unit(
      1.7,
      "cm"
    ),
    
    axis.title = element_text(
      size = 22
    ),
    
    axis.text = element_text(
      size = 20
    ),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    axis.line = element_blank(),
    
    plot.margin = margin(
      t = 15,
      r = 25,
      b = 15,
      l = 25,
      unit = "pt"
    )
  )

print(p)

ggsave(
  filename = out_file,
  plot = p,
  width = 8,
  height = 4.5,
  dpi = 600,
  bg = "white"
)

print(median_df)