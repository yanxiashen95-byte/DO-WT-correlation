
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

input_csv <- "US_1458500.csv"

out_dir <- dirname(input_csv)

df <- read_csv(input_csv, show_col_types = FALSE)
names(df) <- str_trim(names(df))

parse_time_safe <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- str_replace_all(x, "T", " ")
  x <- str_remove(x, "Z$")
  
  parse_date_time(
    x,
    orders = c(
      "ymd HMS",
      "ymd HM",
      "ymd H",
      "ymd",
      "Ymd HMS",
      "Ymd HM",
      "Ymd H",
      "Ymd"
    ),
    tz = "UTC",
    quiet = TRUE
  )
}

df2 <- df %>%
  mutate(
    time = parse_time_safe(timestamp),
    DO = as.numeric(DO),
    WT = as.numeric(WT),
    DO_percent = as.numeric(DO_percent),
    Solar = as.numeric(ALLSKY_SFC_SW_DWN)
  ) %>%
  filter(
    !is.na(time),
    is.finite(DO),
    is.finite(WT),
    is.finite(DO_percent),
    is.finite(Solar)
  ) %>%
  arrange(time) %>%
  select(time, DO, WT, DO_percent, Solar)

plot_df <- df2 %>%
  pivot_longer(
    cols = c(DO, WT, DO_percent, Solar),
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(
    value_min = min(value, na.rm = TRUE),
    value_max = max(value, na.rm = TRUE),
    value_norm = (value - value_min) / (value_max - value_min)
  ) %>%
  ungroup() %>%
  mutate(
    variable = recode(
      variable,
      "Solar" = "Solar radiation",
      "WT" = "WT",
      "DO" = "DO",
      "DO_percent" = "DO%sat"
    ),
    variable = factor(
      variable,
      levels = c("Solar radiation", "WT", "DO", "DO%sat")
    )
  )

check_table <- plot_df %>%
  group_by(variable) %>%
  summarise(
    raw_min = min(value, na.rm = TRUE),
    raw_max = max(value, na.rm = TRUE),
    norm_min = min(value_norm, na.rm = TRUE),
    norm_max = max(value_norm, na.rm = TRUE),
    .groups = "drop"
  )

print(check_table)

p <- ggplot(
  plot_df,
  aes(x = time, y = value_norm, color = variable)
) +
  geom_line(linewidth = 2.1, alpha = 0.85) +
  scale_color_manual(
    values = c(
      "Solar radiation" = "#F2B84B",
      "WT"              = "#E07A3F",
      "DO"              = "#4C78A8",
      "DO%sat"          = "#59A14F"
    )
  ) +
  scale_x_datetime(
    date_breaks = "16 hours",
    date_labels = "%H:%M"
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    x = "Time of day",
    y = "Normalized value",
    color = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 14),
    legend.key.width = unit(1.2, "cm"),
    axis.text.x = element_text(size = 20, color = "black"),
    axis.text.y = element_text(size = 20, color = "black"),
    axis.title.x = element_text(size = 21, color = "black"),
    axis.title.y = element_text(size = 21, color = "black"),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 1
    ),
    axis.line = element_blank()
  )

print(p)

ggsave(
  file.path(out_dir, "US.png"),
  p,
  width = 5,
  height = 4.2,
  dpi = 600
)