#Current Data 

# =========================================================
# 1. Libraries
# =========================================================
library(terra)
library(dplyr)
library(ggplot2)
library(lubridate)
library(patchwork)

# =========================================================
# 2. Paths
# =========================================================
data_path <- "L:/PhD/Escuela Doctoral/Thesis/Thesis/Chapter 1.  Perez et al (2026)/Data/Currents_Global Ocean Physics/Data"
out_path  <- "L:/PhD/Escuela Doctoral/Thesis/Thesis/Chapter 1.  Perez et al (2026)/Data/Currents_Global Ocean Physics/Figures"

setwd(data_path)

if (!dir.exists(out_path)) {
  dir.create(out_path, recursive = TRUE)
}

# =========================================================
# 3. Function to process one current file
# =========================================================
process_currents <- function(file) {
  r <- rast(file)

  u <- r[[grep("uo", names(r), ignore.case = TRUE)]]
  v <- r[[grep("vo", names(r), ignore.case = TRUE)]]

  speed <- sqrt(u^2 + v^2)
  time_vals <- time(u)

  u_mean <- global(u, "mean", na.rm = TRUE)
  v_mean <- global(v, "mean", na.rm = TRUE)
  speed_mean <- global(speed, "mean", na.rm = TRUE)

  df <- data.frame(
    date = as.Date(time_vals),
    u = u_mean[, 1],
    v = v_mean[, 1],
    speed = speed_mean[, 1]
  )

  df$direction <- (atan2(df$v, df$u) * 180 / pi) %% 360
  df
}

# =========================================================
# 4. Process yearly files
#    Replace filenames here if needed
# =========================================================
curr_2016 <- process_currents("cmems_mod_glo_phy_my_0.083deg_P1D-m_1776192763023_2016.nc")
curr_2017 <- process_currents("cmems_mod_glo_phy_my_0.083deg_P1D-m_1776193038759_2017.nc")
curr_2018 <- process_currents("cmems_mod_glo_phy_my_0.083deg_P1D-m_1776193119216_2018.nc")
curr_2019 <- process_currents("cmems_mod_glo_phy_my_0.083deg_P1D-m_1776193177232_2019.nc")
curr_2020 <- process_currents("cmems_mod_glo_phy_my_0.083deg_P1D-m_1776193272297_2020.nc")
curr_2021 <- process_currents("cmems_mod_glo_phy_my_0.083deg_P1D-m_1776193324760_2021.nc")
curr_2022 <- process_currents("cmems_mod_glo_phy_my_0.083deg_P1D-m_1776193374511_2022.nc")
curr_2023 <- process_currents("cmems_mod_glo_phy_my_0.083deg_P1D-m_1776193422607_2023.nc")
curr_2024 <- process_currents("cmems_mod_glo_phy_my_0.083deg_P1D-m_1776193453771_2024.nc")

# =========================================================
# 5. Merge all years
# =========================================================
all_currents <- bind_rows(
  curr_2016,
  curr_2017,
  curr_2018,
  curr_2019,
  curr_2020,
  curr_2021,
  curr_2022,
  curr_2023,
  curr_2024
) %>%
  arrange(date)

# Optional check
table(format(all_currents$date, "%Y"), useNA = "ifany")

# =========================================================
# 6. Summary tables
# =========================================================
annual_stats <- all_currents %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    mean_speed = mean(speed, na.rm = TRUE),
    max_speed  = max(speed, na.rm = TRUE),
    min_speed  = min(speed, na.rm = TRUE),
    sd_speed   = sd(speed, na.rm = TRUE),
    .groups = "drop"
  )

monthly_climatology <- all_currents %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  group_by(month) %>%
  summarise(
    mean_speed = mean(speed, na.rm = TRUE),
    sd_speed   = sd(speed, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================================
# 7. Figure A - Time series
# =========================================================
p1 <- ggplot(all_currents, aes(x = date, y = speed)) +
  geom_line(linewidth = 0.4, alpha = 0.6) +
  geom_smooth(method = "loess", color = "red", linewidth = 0.9, se = FALSE) +
  labs(
    title = "Surface current speed (2016–2024)",
    x = "Year",
    y = "Current speed (m/s)"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9)
  )

# =========================================================
# 8. Figure B - Annual means
# =========================================================
p2 <- ggplot(annual_stats, aes(x = factor(year), y = mean_speed)) +
  geom_col(fill = "#4E79A7", width = 0.7) +
  geom_errorbar(
    aes(ymin = mean_speed - sd_speed, ymax = mean_speed + sd_speed),
    width = 0.2,
    linewidth = 0.4
  ) +
  geom_text(
    aes(label = round(mean_speed, 3)),
    vjust = -0.7,
    size = 2.8
  ) +
  labs(
    title = "Mean annual current speed",
    x = "Year",
    y = "Current speed (m/s)"
  ) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  expand_limits(y = max(annual_stats$mean_speed + annual_stats$sd_speed) + 0.015) +
  ylim(0, 0.35) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    plot.margin = margin(6, 16, 6, 6)
  )

# =========================================================
# 9. Figure C - Monthly climatology
# =========================================================
p3 <- ggplot(monthly_climatology, aes(x = month, y = mean_speed, group = 1)) +
  geom_line(linewidth = 0.8, color = "black") +
  geom_point(size = 2.0, color = "black") +
  geom_errorbar(
    aes(ymin = mean_speed - sd_speed, ymax = mean_speed + sd_speed),
    width = 0.2,
    linewidth = 0.6
  ) +
  labs(
    title = "Monthly climatology",
    x = "Month",
    y = NULL
  ) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  ylim(0, 0.35) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 10),
    plot.margin = margin(6, 6, 6, 16)
  )

# =========================================================
# 10. Combined multi-panel figure
# =========================================================
combined_currents <- p1 / (p2 | p3) +
  plot_layout(heights = c(1, 1.2)) +
  plot_annotation(tag_levels = "A") &
  theme(
    plot.tag = element_text(size = 14, face = "bold"),
    plot.margin = margin(6, 6, 6, 6)
  )

# Preview plots
p1
p2
p3
combined_currents

# =========================================================
# 11. Statistical trend for Figure A
# =========================================================
model <- lm(speed ~ as.numeric(date), data = all_currents)
model_summary <- summary(model)

slope_per_day <- coef(model)[2]
slope_per_year <- slope_per_day * 365
r_squared <- model_summary$r.squared
p_value <- model_summary$coefficients[2, 4]

print(model_summary)
print(slope_per_day)
print(slope_per_year)
print(r_squared)
print(p_value)

# =========================================================
# 12. Save tables
# =========================================================
write.csv(
  all_currents,
  file = file.path(out_path, "currents_2016_2024_combined.csv"),
  row.names = FALSE
)

write.csv(
  annual_stats,
  file = file.path(out_path, "currents_annual_stats_2016_2024.csv"),
  row.names = FALSE
)

write.csv(
  monthly_climatology,
  file = file.path(out_path, "currents_monthly_climatology_2016_2024.csv"),
  row.names = FALSE
)

# =========================================================
# 13. Save individual figures
# =========================================================
ggsave(
  filename = file.path(out_path, "Figure_Currents_TimeSeries_2016_2024_FINAL.png"),
  plot = p1,
  width = 7,
  height = 4.5,
  dpi = 600
)

ggsave(
  filename = file.path(out_path, "Figure_Currents_TimeSeries_2016_2024_FINAL.pdf"),
  plot = p1,
  width = 7,
  height = 4.5
)

ggsave(
  filename = file.path(out_path, "Figure_Currents_Annual_Mean_FINAL.png"),
  plot = p2,
  width = 7,
  height = 4.5,
  dpi = 600
)

ggsave(
  filename = file.path(out_path, "Figure_Currents_Annual_Mean_FINAL.pdf"),
  plot = p2,
  width = 7,
  height = 4.5
)

ggsave(
  filename = file.path(out_path, "Figure_Currents_Seasonal_Climatology_FINAL.png"),
  plot = p3,
  width = 7,
  height = 4.5,
  dpi = 600
)

ggsave(
  filename = file.path(out_path, "Figure_Currents_Seasonal_Climatology_FINAL.pdf"),
  plot = p3,
  width = 7,
  height = 4.5
)

# =========================================================
# 14. Save combined figure
# =========================================================
ggsave(
  filename = file.path(out_path, "Figure_Currents_Multipanel_ABC_FINAL.png"),
  plot = combined_currents,
  width = 14,
  height = 9,
  dpi = 600
)

ggsave(
  filename = file.path(out_path, "Figure_Currents_Multipanel_ABC_FINAL.pdf"),
  plot = combined_currents,
  width = 14,
  height = 9
)

#confirm the sd of figure c for june 
all_currents %>%
  filter(month(date) == 6) %>%
  summarise(
    mean = mean(speed),
    sd = sd(speed),
    min = min(speed),
    max = max(speed)
  )