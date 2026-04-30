# =========================
# S2 FAI Results - Step 1
# =========================

library(readxl)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(readr)

# Confirm we are in the project folder
getwd()
list.files()

xlsx <- "S2_FAI.xlsx"
stopifnot(file.exists(xlsx))

# List the sheets
excel_sheets(xlsx)

# =========================
# Step 2: Import LPNMR
# =========================

lpnmr <- read_excel(xlsx, sheet = "LPNMR Area Coverage") %>%
  clean_names() %>%
  mutate(date = as.Date(date))

# Quick structure check
names(lpnmr)
glimpse(lpnmr)
head(lpnmr, 10)

# =========================
# Step 3: QC checks
# =========================

# 1. Missing values
colSums(is.na(lpnmr))

# 2. Duplicate dates
lpnmr %>%
  count(date) %>%
  filter(n > 1)

# 3. m2 -> km2 verification
lpnmr %>%
  mutate(check_km2 = pixel_count_m2 / 1e6,
         diff = check_km2 - pixel_count_km2) %>%
  summarise(max_abs_diff = max(abs(diff), na.rm = TRUE))

# =========================
# Step 4: Generate Results Tables
# =========================

dir.create("outputs", showWarnings = FALSE)

# 1. Save clean daily dataset
write_csv(lpnmr, "outputs/LPNMR_daily_clean.csv")

# 2. Monthly summary
lpnmr_monthly <- lpnmr %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    n_scenes = n(),
    area_km2_sum = sum(pixel_count_km2, na.rm = TRUE),
    area_km2_mean = mean(pixel_count_km2, na.rm = TRUE),
    area_km2_median = median(pixel_count_km2, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(lpnmr_monthly, "outputs/LPNMR_monthly_summary.csv")

# 3. Annual summary
lpnmr_annual <- lpnmr %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    n_scenes = n(),
    area_km2_sum = sum(pixel_count_km2, na.rm = TRUE),
    area_km2_mean = mean(pixel_count_km2, na.rm = TRUE),
    area_km2_median = median(pixel_count_km2, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(lpnmr_annual, "outputs/LPNMR_annual_summary.csv")

#Verify ranges:
range(lpnmr$date)

range(lpnmr$pixel_count_km2)

summary(lpnmr_annual$area_km2_sum)

# =========================
# Step 4: Restrict study period
# =========================

lpnmr_filtered <- lpnmr %>%
  filter(date >= as.Date("2016-01-01"),
         date <= as.Date("2024-12-31"))

# Verify new range
range(lpnmr_filtered$date)

nrow(lpnmr_filtered)

# Recompute monthly
lpnmr_monthly <- lpnmr_filtered %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    n_scenes = n(),
    area_km2_sum = sum(pixel_count_km2, na.rm = TRUE),
    area_km2_mean = mean(pixel_count_km2, na.rm = TRUE),
    area_km2_median = median(pixel_count_km2, na.rm = TRUE),
    .groups = "drop"
  )

# Recompute annual
lpnmr_annual <- lpnmr_filtered %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    n_scenes = n(),
    area_km2_sum = sum(pixel_count_km2, na.rm = TRUE),
    area_km2_mean = mean(pixel_count_km2, na.rm = TRUE),
    area_km2_median = median(pixel_count_km2, na.rm = TRUE),
    .groups = "drop"
  )

lpnmr_annual

# =========================
# Step 5: Manuscript Annual Table
# =========================

manuscript_table <- lpnmr_annual %>%
  select(year, area_km2_sum) %>%
  mutate(
    area_km2_sum = round(area_km2_sum, 2)
  ) %>%
  rename(
    Year = year,
    `Total Sargassum Area (km²)` = area_km2_sum
  )

manuscript_table

#Export table for Word
write_csv(manuscript_table, "outputs/Table_Annual_LPNMR_2016_2024.csv")

# More detailed table
manuscript_table_full <- lpnmr_annual %>%
  mutate(
    area_km2_sum = round(area_km2_sum, 2),
    area_km2_mean = round(area_km2_mean, 3),
    area_km2_median = round(area_km2_median, 3)
  ) %>%
  rename(
    Year = year,
    `Number of Scenes` = n_scenes,
    `Annual Total (km²)` = area_km2_sum,
    `Mean per Scene (km²)` = area_km2_mean,
    `Median per Scene (km²)` = area_km2_median
  )

manuscript_table_full

#Monthly Summary for LPNMR_Filtered (2016-2024)
lpnmr_monthly <- lpnmr_filtered %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    n_scenes = n(),
    area_km2_sum = sum(pixel_count_km2, na.rm = TRUE),
    area_km2_mean = mean(pixel_count_km2, na.rm = TRUE),
    area_km2_median = median(pixel_count_km2, na.rm = TRUE),
    area_km2_max = max(pixel_count_km2, na.rm = TRUE),   # <-- add this
    .groups = "drop"
  )

write_csv(lpnmr_monthly, "outputs/LPNMR_monthly_summary_2016_2024.csv")

# =========================
# Monthly Metrics (2016–2024)
# step 1
# =========================

lpnmr_monthly <- lpnmr_filtered %>%
  mutate(month = floor_date(date, "month"),
         year = year(date)) %>%
  group_by(year, month) %>%
  summarise(
    n_scenes = n(),
    area_km2_sum = sum(pixel_count_km2, na.rm = TRUE),      # cumulative exposure
    area_km2_mean = mean(pixel_count_km2, na.rm = TRUE),    # typical condition
    area_km2_median = median(pixel_count_km2, na.rm = TRUE),
    area_km2_max = max(pixel_count_km2, na.rm = TRUE),      # peak event
    .groups = "drop"
  )

head(lpnmr_monthly)

#Step 2
#Identify Peak monthly event (Across entire study)
lpnmr_monthly %>%
  arrange(desc(area_km2_max)) %>%
  head(5)

#Step 3
#Seasonal pattern (Across all years)
seasonal_pattern <- lpnmr_monthly %>%
  mutate(month_num = month(month)) %>%
  group_by(month_num) %>%
  summarise(
    mean_monthly_max = mean(area_km2_max, na.rm = TRUE),
    mean_monthly_mean = mean(area_km2_mean, na.rm = TRUE),
    mean_monthly_sum = mean(area_km2_sum, na.rm = TRUE),
    .groups = "drop"
  )

seasonal_pattern

#Step 4
#Annual Cumulative Exposure (already done)
lpnmr_annual

#Identify the strongest monthly peak in the entire dataset
lpnmr_monthly %>%
  arrange(desc(area_km2_max)) %>%
  head(10)

#Identify Seasonal PAtterns
seasonal_pattern <- lpnmr_monthly %>%
  mutate(month_num = month(month)) %>%
  group_by(month_num) %>%
  summarise(
    mean_max = mean(area_km2_max, na.rm = TRUE),
    mean_mean = mean(area_km2_mean, na.rm = TRUE),
    mean_sum = mean(area_km2_sum, na.rm = TRUE),
    .groups = "drop"
  )

seasonal_pattern


# =========================
# Figure 1: Annual Totals (Bar Plots)
# =========================

p_annual <- ggplot(lpnmr_annual, 
                   aes(x = factor(year), 
                       y = area_km2_sum)) +
  geom_col(fill = "black", width = 0.7) +
  labs(
    x = "Year",
    y = expression("Total Area (km"^2*")")
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_blank()
  )

ggsave("outputs/Fig1_Annual_Totals_Journal.png",
       p_annual,
       width = 7,
       height = 5,
       dpi = 300)

#  Figure 1: Annual Totals (Bar Plots) third edit graph
p_annual <- ggplot(lpnmr_annual, 
                   aes(x = factor(year), 
                       y = area_km2_sum)) +
  geom_col(fill = "black", width = 0.7) +
  labs(
    x = "Year",
    y = expression("Total Area (km"^2*")")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    axis.line = element_line(size = 0.8),
    axis.ticks = element_line(size = 0.8)
  )

ggsave("outputs/Fig1_Annual_Totals_Final.png",
       p_annual,
       width = 7,
       height = 5,
       dpi = 300)

# =========================
# Figure 1: Interannual Variability (Color + labels + SD)
# =========================
#Interannual Figure

# Restrict study period (2016–2024)
lpnmr_filtered <- lpnmr %>%
  filter(date >= as.Date("2016-01-01"),
         date <= as.Date("2024-12-31"))

ls()

# Recompute annual summary including SD
lpnmr_annual <- lpnmr_filtered %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    n_scenes = n(),
    area_km2_sum = sum(pixel_count_km2, na.rm = TRUE),
    area_km2_mean = mean(pixel_count_km2, na.rm = TRUE),
    area_km2_median = median(pixel_count_km2, na.rm = TRUE),
    area_km2_sd = sd(pixel_count_km2, na.rm = TRUE),
    .groups = "drop"
  )

lpnmr_annual

# Plot annual totals with color, SD bars, and total labels
p_annual <- ggplot(lpnmr_annual,
                   aes(x = factor(year), y = area_km2_sum, fill = factor(year))) +
  geom_col(width = 0.7) +

  # Labels above bars
  geom_text(aes(label = round(area_km2_sum, 1)),
            vjust = -0.6,
            size = 4) +

  # Optional SD bars (see note below)
  geom_errorbar(aes(ymin = area_km2_sum - area_km2_sd,
                    ymax = area_km2_sum + area_km2_sd),
                width = 0.2,
                linewidth = 0.5) +

  labs(
    x = "Year",
    y = expression("Total Surface Sargassum Area (km"^2*")")
  ) +

  scale_fill_viridis_d(option = "plasma", guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +

  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "plain"),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.8)
  )

p_annual

ggsave("outputs/Fig1_Annual_Totals_Color_Final.png",
       p_annual,
       width = 7,
       height = 5,
       dpi = 600)

scale_fill_viridis_d(option = "C", guide = "none")

p_annual <- ggplot(lpnmr_annual,
                   aes(x = factor(year),
                       y = area_km2_sum,
                       fill = factor(year))) +

geom_col(width = 0.7, color = "black", linewidth = 0.3) +

  geom_text(aes(label = round(area_km2_sum,1)),
            vjust = -0.6,
            size = 4) +

  labs(
    x = "Year",
    y = expression("Total Surface Sargassum Area (km"^2*")")
  ) +

  scale_fill_viridis_d(option = "C", guide = "none") +

  scale_y_continuous(expand = expansion(mult = c(0,0.1))) +

  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color="black"),
    axis.title = element_text(face="plain"),
    axis.line = element_line(linewidth=0.8),
    axis.ticks = element_line(linewidth=0.8)
  )

ggsave("outputs/Fig1_Annual_Totals_Final.png",
       p_annual,
       width = 7,
       height = 5,
       dpi = 600)

# =========================
# Figure 2: Daily Time Series
# =========================

p_daily <- ggplot(lpnmr_filtered,
                  aes(x = date,
                      y = pixel_count_km2)) +

  # Raw daily signal
  geom_line(color = "grey40", size = 0.4) +

  # Smoothed trend
  geom_smooth(method = "loess",
              span = 0.2,
              color = "black",
              size = 1.2,
              se = FALSE) +

  labs(
    x = "Year",
    y = expression("Sargassum Area (km"^2*")")
  ) +

  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    axis.line = element_line(size = 0.8),
    axis.ticks = element_line(size = 0.8)
  )

ggsave("outputs/Fig2_Daily_TimeSeries_Final.png",
       p_daily,
       width = 10,
       height = 4.5,
       dpi = 300)

p_daily

# Daily Time Series (second try)
p_daily <- ggplot(lpnmr_filtered,
                  aes(x = date,
                      y = pixel_count_km2)) +
  
  geom_line(color = "grey40", linewidth = 0.4) +
  
  geom_smooth(method = "loess",
              span = 0.2,
              color = "black",
              linewidth = 1.2,
              se = FALSE) +
  
  labs(
    x = "Year",
    y = expression("Sargassum Area (km"^2*")")
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.8)
  )


# =========================
# Figure 3: Seasonal Climatology
# =========================

seasonal_pattern <- lpnmr_monthly %>%
  mutate(month_num = month(month)) %>%
  group_by(month_num) %>%
  summarise(
    mean_max = mean(area_km2_max, na.rm = TRUE),
    .groups = "drop"
  )

p_seasonal <- ggplot(seasonal_pattern,
                     aes(x = month_num,
                         y = mean_max)) +

  geom_line(color = "black", linewidth = 1.2) +
  geom_point(size = 2, color = "black") +

  scale_x_continuous(breaks = 1:12) +

  labs(
    x = "Month",
    y = expression("Mean Monthly Maximum Area (km"^2*")")
  ) +

  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.8)
  )

ggsave("outputs/Fig3_Seasonal_Climatology_Final.png",
       p_seasonal,
       width = 7,
       height = 5,
       dpi = 300)

p_seasonal

#Seasonal Climatology_Final with monthly names

lpnmr_monthly <- lpnmr %>%
  mutate(month = lubridate::month(date)) %>%
  group_by(year = lubridate::year(date), month) %>%
  summarise(
    area_km2_max = max(pixel_count_km2, na.rm = TRUE),
    .groups = "drop"
  )
seasonal_pattern <- lpnmr_monthly %>%
  mutate(
    month_num = month(month),
    month_label = factor(month_num,
                         levels = 1:12,
                         labels = c("Jan","Feb","Mar","Apr","May","Jun",
                                    "Jul","Aug","Sep","Oct","Nov","Dec"))
  ) %>%
  group_by(month_label) %>%
  summarise(
    mean_max = mean(area_km2_max, na.rm = TRUE),
    .groups = "drop"
  )

p_seasonal <- ggplot(seasonal_pattern,
                     aes(x = month_label,
                         y = mean_max)) +

  geom_line(aes(group = 1), color = "black", linewidth = 1.2) +
  geom_point(size = 3, color = "black") +

  labs(
    x = "Month",
    y = expression("Mean Monthly Maximum Area (km"^2*")")
  ) +

  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(face = "plain"),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.8)
  )
ggsave("outputs/Fig3_Seasonal_Climatology_Final.png",
       p_seasonal,
       width = 7,
       height = 5,
       dpi = 300)


p_seasonal

#p_Daily values code replace for manuscript
p_daily <- ggplot(lpnmr_filtered, aes(x = date, y = pixel_count_km2)) +

  geom_line(color = "grey50", linewidth = 0.30) +

  geom_smooth(
    method = "loess",
    span = 0.20,
    se = FALSE,
    color = "black",
    linewidth = 1.20
  ) +

  scale_x_date(
    breaks = seq(as.Date("2016-01-01"),
                 as.Date("2024-01-01"),
                 by = "1 year"),
    date_labels = "%Y"
  ) +

  labs(
    x = "Year",
    y = expression("Sargassum Area (km"^2*")")
  ) +

  theme_classic(base_size = 14) +
  theme(
    axis.text  = element_text(color = "black"),
    axis.title = element_text(face = "bold"),
    axis.line  = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.8)
  )

p_daily

ggsave(
  filename = "outputs/Fig2_Daily_TimeSeries_2016_2024.png",
  plot     = p_daily,
  width    = 9,
  height   = 5,
  dpi      = 300
)

#LPNMR Quantification 
lpnmr_quant <- lpnmr_filtered %>%
  summarise(
    min_km2 = min(pixel_count_km2, na.rm = TRUE),
    max_km2 = max(pixel_count_km2, na.rm = TRUE),
    mean_km2 = mean(pixel_count_km2, na.rm = TRUE),
    median_km2 = median(pixel_count_km2, na.rm = TRUE),
    total_km2 = sum(pixel_count_km2, na.rm = TRUE)
  )

lpnmr_quant

rm(list = ls())


# =====================================================
# 4. Site-Specific Sargassum Accumulation Patterns
# =====================================================

getwd()
list.files()

#Create Output folder
dir.create("outputs", showWarnings = FALSE)

#Step 2 - Identify the subsites sheets
xlsx <- "S2_FAI.xlsx"
all_sheets <- excel_sheets(xlsx)
all_sheets

#Define only the site sheets (exclude LPNMR)
site_sheets <- setdiff(all_sheets, "LPNMR Area Coverage")
site_sheets

#Step 3 - read all site sheets + merge into one dataset
library(readxl)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)

start_date <- as.Date("2016-01-01")
end_date   <- as.Date("2024-12-31")

sites_all <- lapply(site_sheets, function(sh){

  df <- read_excel(xlsx, sheet = sh) %>%
    clean_names()

  # Convert date safely (works if Excel stored it as date or text)
  if ("date" %in% names(df)) {
    df <- df %>% mutate(date = as.Date(date))
  }

  df %>%
    mutate(
      site = sh,
      site_code = str_extract(sh, "\\([0-9][A-Z]\\)") %>% str_replace_all("[()]", "")
    ) %>%
    filter(date >= start_date, date <= end_date)
})

sites_all <- bind_rows(sites_all)

glimpse(sites_all)

#Step 4 - Compute site level summary statistic (km2)
site_summary <- sites_all %>%
  group_by(site, site_code) %>%
  summarise(
    n_scenes = n(),
    total_km2 = sum(pixel_count_km2, na.rm = TRUE),
    mean_km2  = mean(pixel_count_km2, na.rm = TRUE),
    median_km2 = median(pixel_count_km2, na.rm = TRUE),
    min_km2   = min(pixel_count_km2, na.rm = TRUE),
    max_km2   = max(pixel_count_km2, na.rm = TRUE),

    # Detection frequency (scenes where detected area > 0)
    n_detect  = sum(pixel_count_km2 > 0, na.rm = TRUE),
    detect_freq = 100 * n_detect / n_scenes,

    .groups = "drop"
  ) %>%
  arrange(desc(total_km2))

site_summary

#Step 5 - Convert site summary from km2 to m2
site_summary_m2 <- site_summary %>%
  mutate(
    total_m2  = total_km2  * 1e6,
    mean_m2   = mean_km2   * 1e6,
    median_m2 = median_km2 * 1e6,
    min_m2    = min_km2    * 1e6,
    max_m2    = max_km2    * 1e6
  ) %>%
  select(site, site_code, n_scenes,
         total_m2, mean_m2, median_m2, min_m2, max_m2,
         n_detect, detect_freq) %>%
  arrange(desc(total_m2))

site_summary_m2

# ============================================================
# FIX: Force plotting order (swap 1B and 2A only)
# ============================================================

site_summary_m2 <- site_summary_m2 %>%
  mutate(site = factor(site, levels = rev(c(
    "Isla Guayacan (1A)",
    "Isla Cueva (1B)",
    "Isla Cueva (2A)",
    "Vieques Cay (2B)",
    "Maria 1 Cay (3A)",
    "Maria 2 Cay (3B)"
  ))))


#Step 6 - Export site table (m2) to output
write.csv(site_summary_m2, "outputs/Table_SiteSummary_m2_2016_2024.csv", row.names = FALSE)

#Step 7 - Make the site figure (cumulative m2 by site)
library(ggplot2)

p_site_total_m2 <- ggplot(site_summary_m2,
                          aes(x = site, y = total_m2)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(
    x = "Sites of Interest",   # ← THIS ADDS YOUR SITE AXIS TITLE
    y = expression("Surface Sargassum Area (m"^2*")")
  ) +
  theme_classic(base_size = 14)

p_site_total_m2


ggsave(
  filename = "outputs/Fig4_Site_Cumulative_m2.png",
  plot = p_site_total_m2,
  width = 7,
  height = 4.5,
  dpi = 600
)

## Step 7.  Site Specific Accumulation (paper quality)

library(ggplot2)
library(dplyr)
library(scales)

# Add zone classification
site_summary_m2 <- site_summary_m2 %>%
  mutate(
    zone = case_when(
      site %in% c("Isla Guayacan (1A)", "Isla Cueva (1B)") ~ "Inner",
      site %in% c("Isla Cueva (2A)", "Vieques Cay (2B)") ~ "Intermediate",
      site %in% c("Maria 1 Cay (3A)", "Maria 2 Cay (3B)") ~ "Outer"
    )
  )

p_site_total_m2 <- ggplot(site_summary_m2,
                          aes(x = site, y = total_m2, fill = zone)) +
  geom_col(width = 0.72, color = "black", linewidth = 0.3) +
  coord_flip() +

  geom_text(aes(label = comma(round(total_m2, 0))),
            hjust = -0.1,
            size = 4) +

  scale_fill_manual(values = c(
    "Inner" = "#1f78b4",
    "Intermediate" = "#33a02c",
    "Outer" = "#ff7f00"
  )) +

  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.10))
  ) +

  labs(
    x = NULL,
    y = expression("Cumulative Surface Sargassum Area (m"^2*")"),
    fill = "Accumulation Zone"
  ) +

  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", face = "bold"),
    axis.line = element_line(linewidth = 0.8),
    axis.ticks = element_line(linewidth = 0.8),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(color = "black")
  )

p_site_total_m2

ggsave(
  filename = "outputs/Fig4_Site_Cumulative_m2_publication.png",
  plot = p_site_total_m2,
  width = 8,
  height = 5,
  dpi = 600
)


#Step 8 - Quick check
range(site_summary_m2$total_m2)
site_summary_m2 %>% select(site, total_m2, detect_freq)


##Suplementary Table for site summary m2 and detection frequency per site

library(dplyr)
library(readr)

# --- Supplementary table: total area (m²) + detection frequency (%)
supp_table_sites <- site_summary_m2 %>%
  mutate(
    total_m2_round = round(total_m2, 0),
    detect_freq_round = round(detect_freq, 1),

    # Optional: percent contribution to total across the 6 sites
    percent_of_sites_total = 100 * total_m2 / sum(total_m2, na.rm = TRUE),
    percent_of_sites_total = round(percent_of_sites_total, 1),

    # This creates the exact "288 m² (9.6%)" style string
    total_and_detect = paste0(format(total_m2_round, big.mark = ","), " m² (",
                             detect_freq_round, "%)")
  ) %>%
  select(
    site, site_code, n_scenes,
    total_m2 = total_m2_round,
    detect_freq = detect_freq_round,
    percent_of_sites_total,
    total_and_detect
  )

supp_table_sites

# Export to outputs for Supplementary Materials
write_csv(supp_table_sites, "outputs/Table_S2_SiteTotals_m2_andDetectionFreq.csv")



# ============================================================
# Step 10 — Classify High vs Low Accumulation Sites
# ============================================================

sites_all_grouped <- sites_all %>%
  mutate(
    accumulation_group = case_when(
      site_code %in% c("1A","1B","2A") ~ "High",
      site_code %in% c("2B","3A","3B") ~ "Low"
    )
  )

# Convert km2 to m2 at scene level for analysis
# Total area, mean per scene, median per scene, detection frequency
sites_all_grouped <- sites_all_grouped %>%
  mutate(pixel_count_m2 = pixel_count_km2 * 1e6)

group_summary <- sites_all_grouped %>%
  group_by(accumulation_group) %>%
  summarise(
    n_scenes = n(),
    total_m2 = sum(pixel_count_m2, na.rm = TRUE),
    mean_m2 = mean(pixel_count_m2, na.rm = TRUE),
    median_m2 = median(pixel_count_m2, na.rm = TRUE),
    detect_freq = 100 * sum(pixel_count_m2 > 0) / n(),
    .groups = "drop"
  )

group_summary

#Percent contribuition by gruop
group_summary <- group_summary %>%
  mutate(
    percent_contribution = 100 * total_m2 / sum(total_m2),
    percent_contribution = round(percent_contribution, 1)
  )

group_summary

#aCCUMULATION gRUOPS Boxplot
library(ggplot2)

p_high_low <- ggplot(sites_all_grouped,
                     aes(x = accumulation_group,
                         y = pixel_count_m2,
                         fill = accumulation_group)) +
  geom_boxplot(outlier.alpha = 0.3) +
  labs(
    x = "Accumulation Zone Groups",
    y = expression("Surface Sargassum Area per Scene (m"^2*")")
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

p_high_low

ggsave("outputs/Fig5_High_vs_Low_Boxplot.png",
       p_high_low,
       width = 6,
       height = 4.5,
       dpi = 600)


# Find the date with maximum Sargassum area (km2)

ls()

ls()

names(sites_all)
names(sites_all_grouped)
names(site_summary)

#Summary of all sites per date

library(readxl)
library(dplyr)
library(lubridate)

# 1) list sheet names (so you pick the exact one)
excel_sheets(xlsx)

# 2) load ONLY the LPNMR sheet
lpnmr <- read_excel(xlsx, sheet = "LPNMR Area Coverage") %>%
  janitor::clean_names()

# check
names(lpnmr)
head(lpnmr)
nrow(lpnmr)

library(readxl)
library(dplyr)
library(lubridate)

# 1) list sheet names (so you pick the exact one)
excel_sheets(xlsx)

# 2) load ONLY the LPNMR sheet
lpnmr <- read_excel(xlsx, sheet = "LPNMR Area Coverage") %>%
  janitor::clean_names()

# check
names(lpnmr)
head(lpnmr)
nrow(lpnmr)

names(lpnmr)

lpnmr <- lpnmr %>% mutate(date = as.Date(date))

max_event_lpnmr <- lpnmr %>%
  arrange(desc(pixel_count_km2)) %>%
  slice(1)

max_event_lpnmr

lpnmr %>%
  arrange(desc(pixel_count_km2)) %>%
  select(date, pixel_count_km2) %>%
  slice(1:10)

#detection frequency
total_scenes <- nrow(lpnmr)

detected_scenes <- sum(lpnmr$pixel_count_km2 > 0, na.rm = TRUE)

detection_frequency <- (detected_scenes / total_scenes) * 100

total_scenes
detected_scenes
detection_frequency

detected_scenes <- sum(lpnmr$pixel_count_km2 > 0.1, na.rm = TRUE)

detection_frequency <- (detected_scenes / total_scenes) * 100

detected_scenes
detection_frequency

#Standard deviation of Sargassum area for the LPNMR
sd_area <- sd(lpnmr$pixel_count_km2, na.rm = TRUE)

sd_area

mean_area <- mean(lpnmr$pixel_count_km2, na.rm = TRUE)

mean_area

min(lpnmr$pixel_count_km2, na.rm = TRUE)
max(lpnmr$pixel_count_km2, na.rm = TRUE)

quantile(lpnmr$pixel_count_km2, probs = c(0.25, 0.75), na.rm = TRUE)

##High and low accumulations for each group
#Quantitive analysis

sites_all_grouped %>%
  group_by(accumulation_group) %>%
  summarise(median_area = median(pixel_count_m2, na.rm = TRUE))

#Ratio
medians <- sites_all_grouped %>%
  group_by(accumulation_group) %>%
  summarise(median_area = median(pixel_count_m2, na.rm = TRUE))

ratio <- medians$median_area[medians$accumulation_group=="High"] /
         medians$median_area[medians$accumulation_group=="Low"]

ratio


library(dplyr)
library(ggplot2)

# Top 10 extreme events from the LPNMR sheet
top10_events <- lpnmr %>%
  arrange(desc(pixel_count_km2)) %>%
  select(date, pixel_count_km2) %>%
  slice(1:10) %>%
  mutate(date_label = format(date, "%Y-%m-%d"))

top10_events

# Plot
p_top10_events <- ggplot(top10_events,
                         aes(x = reorder(date_label, pixel_count_km2),
                             y = pixel_count_km2)) +
  geom_col(width = 0.7) +
  coord_flip() +
  labs(x = "Date",
       y = expression("Detected Sargassum Surface Area (km"^2*")")) +
  theme_classic(base_size = 14)

p_top10_events

# Save
ggsave("outputs/Fig_Extreme_Events_Top10_LPNMR.png",
       p_top10_events,
       width = 7,
       height = 4.8,
       dpi = 600)




# Select top 10 events
top10_events <- lpnmr %>%
  arrange(desc(pixel_count_km2)) %>%
  select(date, pixel_count_km2) %>%
  slice(1:10) %>%
  mutate(date_label = format(date, "%Y-%m-%d"))

top10_events

# Plot
p_top10_events <- ggplot(top10_events, aes(x = reorder(date_label, pixel_count_km2), y = pixel_count_km2)) +
  geom_col(width = 0.7, fill = "steelblue") +
  
  # Add value labels
  geom_text(aes(label = round(pixel_count_km2, 2)), hjust = -0.1, size = 4) +
  
  coord_flip() +
  
  labs(x = "Date",
       y = expression("Detected Sargassum Surface Area (km"^2 * ")")) +
  
  theme_classic(base_size = 14)

p_top10_events

# Save
ggsave(
  "outputs/Fig_Extreme_Events_Top10_LPNMR.png",
  p_top10_events,
  width = 7,
  height = 4.8,
  dpi = 600
)


