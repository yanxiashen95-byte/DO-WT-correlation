library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# ============================================================
# 1. Input and output files
# ============================================================

low_file <- "all_sites_low_flow_r_slope_summary.csv"
normal_file <- "all_sites_normal_flow_r_slope_summary.csv"
high_file <- "all_sites_high_flow_r_slope_summary.csv"

out_file <- "r_median_CDF_black_grey.png"

# ============================================================
# 2. Read data
# ============================================================

low_df <- read_csv(
  low_file,
  show_col_types = FALSE
) %>%
  mutate(flow_group = "Low-flow")

normal_df <- read_csv(
  normal_file,
  show_col_types = FALSE
) %>%
  mutate(flow_group = "Normal-flow")

high_df <- read_csv(
  high_file,
  show_col_types = FALSE
) %>%
  mutate(flow_group = "High-flow")

# ============================================================
# 3. Combine and clean data
# ============================================================

plot_df <- bind_rows(
  low_df,
  normal_df,
  high_df
) %>%
  mutate(
    r_median = as.numeric(r_median),
    
    flow_group = factor(
      flow_group,
      levels = c(
        "Low-flow",
        "Normal-flow",
        "High-flow"
      )
    )
  ) %>%
  filter(
    !is.na(r_median),
    is.finite(r_median),
    r_median >= -1,
    r_median <= 1
  )

# ============================================================
# 4. Calculate summary statistics
# ============================================================

summary_df <- plot_df %>%
  group_by(flow_group) %>%
  summarise(
    n = n(),
    
    q25 = quantile(
      r_median,
      probs = 0.25,
      na.rm = TRUE
    ),
    
    median_r = median(
      r_median,
      na.rm = TRUE
    ),
    
    q75 = quantile(
      r_median,
      probs = 0.75,
      na.rm = TRUE
    ),
    
    negative_percent = mean(
      r_median < 0,
      na.rm = TRUE
    ) * 100,
    
    .groups = "drop"
  )

print(summary_df)

# ============================================================
# 5. Black-grey color scheme
# ============================================================

flow_colors <- c(
  "Low-flow"    = "#BDBDBD",  # light grey
  "Normal-flow" = "#707070",  # dark grey
  "High-flow"   = "grey25"   # black
)

# ============================================================
# 6. Plot CDF
# ============================================================

p <- ggplot(
  plot_df,
  aes(
    x = r_median,
    color = flow_group,
    group = flow_group
  )
) +
  
  # Zero-correlation reference line
  geom_vline(
    xintercept = 0,
    color = "grey55",
    linetype = "dotted",
    linewidth = 0.8
  ) +
  
  # Reference line for the median
  geom_hline(
    yintercept = 0.5,
    color = "grey75",
    linetype = "dotted",
    linewidth = 0.7
  ) +
  
  # Empirical cumulative distribution curves
  stat_ecdf(
    geom = "step",
    linetype = "solid",
    linewidth = 2,
    pad = TRUE
  ) +
  # Vertical dashed lines indicating group medians
  geom_segment(
    data = summary_df,
    aes(
      x = median_r,
      xend = median_r,
      y = 0,
      yend = 0.5,
      color = flow_group
    ),
    inherit.aes = FALSE,
    linetype = "dashed",
    linewidth = 0.9,
    show.legend = FALSE
  ) +
  # Median points
  geom_point(
    data = summary_df,
    aes(
      x = median_r,
      y = 0.5,
      color = flow_group
    ),
    inherit.aes = FALSE,
    shape = 21,
    fill = "white",
    stroke = 1.2,
    size = 3.5,
    show.legend = FALSE
  ) +
  
  # Black-grey colors
  scale_color_manual(
    values = flow_colors,
    breaks = c(
      "Low-flow",
      "Normal-flow",
      "High-flow"
    )
  ) +
  
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = seq(
      -1,
      1,
      by = 0.5
    ),
    expand = expansion(
      mult = c(0.01, 0.01)
    )
  ) +
  
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(
      0,
      1,
      by = 0.2
    ),
    labels = label_percent(
      accuracy = 1
    ),
    expand = expansion(
      mult = c(0, 0.02)
    )
  ) +
  
  labs(
    x = "Median DO–WT correlation",
    y = "Cumulative proportion",
    color = NULL
  ) +
  
  theme_classic(
    base_size = 14
  ) +
  
  theme(
    legend.position = "right",
    
    legend.text = element_text(
      size = 20,
      color = "black"
    ),
    
    legend.key.width = grid::unit(
      1.8,
      "cm"
    ),
    
    axis.title.x = element_text(
      size = 22,
      color = "black"
    ),
    
    axis.title.y = element_text(
      size = 22,
      color = "black"
    ),
    
    axis.text = element_text(
      size = 20,
      color = "black"
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
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = "solid",
        linewidth = 2
      )
    )
  )

print(p)

# ============================================================
# 7. Save figure
# ============================================================

ggsave(
  filename = out_file,
  plot = p,
  width = 8,
  height = 4.5,
  dpi = 600,
  bg = "white"
)