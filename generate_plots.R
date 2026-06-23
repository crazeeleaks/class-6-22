# Portland, Oregon Daily Maximum Temperatures Beeswarm Visualization (1979-2021)
# This script processes historical GHCN-Daily data for Portland International Airport (USW00024229)
# and creates high-resolution beeswarm plots using ggplot2 and ggbeeswarm.

# -------------------------------------------------------------------------
# 1. Setup and Dependencies
# -------------------------------------------------------------------------
cat("Setting up dependencies...\n")
required_packages <- c("ggplot2", "dplyr", "lubridate", "ggbeeswarm", "scales")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(new_packages) > 0) {
  cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
  # Use pak if available for faster installation, otherwise install.packages
  if ("pak" %in% installed.packages()[, "Package"]) {
    pak::pkg_install(new_packages)
  } else {
    install.packages(new_packages, repos = "https://cloud.r-project.org")
  }
}

library(ggplot2)
library(dplyr)
library(lubridate)
library(ggbeeswarm)
library(scales)

# -------------------------------------------------------------------------
# 2. Data Retrieval
# -------------------------------------------------------------------------
csv_filename <- "portland_weather_raw.csv"
noaa_url <- "https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/USW00024229.csv"

if (!file.exists(csv_filename)) {
  cat("Local weather data file not found. Downloading from NOAA (12MB)...\n")
  download.file(noaa_url, destfile = csv_filename, mode = "wb")
  cat("Download complete!\n")
} else {
  cat("Loading cached data from:", csv_filename, "\n")
}

# -------------------------------------------------------------------------
# 3. Data Processing and Cleaning
# -------------------------------------------------------------------------
cat("Processing temperature records...\n")
raw_data <- read.csv(csv_filename, stringsAsFactors = FALSE)

# Filter for the requested period (1979-2021) and clean the data
weather_data <- raw_data %>%
  mutate(DATE = as.Date(DATE)) %>%
  filter(DATE >= as.Date("1979-01-01") & DATE <= as.Date("2021-12-31")) %>%
  filter(!is.na(TMAX)) %>%
  mutate(
    Year = year(DATE),
    MonthNum = month(DATE),
    # Create ordered factor for months
    Month = month(DATE, label = TRUE, abbr = FALSE),
    Day = day(DATE),
    # Convert GHCN standard (tenths of degrees Celsius) to Celsius and Fahrenheit
    TMAX_C = TMAX / 10,
    TMAX_F = TMAX_C * 1.8 + 32,
    # Define meteorological seasons
    Season = case_when(
      MonthNum %in% c(12, 1, 2) ~ "Winter",
      MonthNum %in% c(3, 4, 5) ~ "Spring",
      MonthNum %in% c(6, 7, 8) ~ "Summer",
      MonthNum %in% c(9, 10, 11) ~ "Autumn"
    ) %>% factor(levels = c("Winter", "Spring", "Summer", "Autumn"))
  )

# Verify dataset structure and print basic statistics
cat(sprintf("Successfully processed %d daily temperature records.\n", nrow(weather_data)))
cat(sprintf("Date range: %s to %s\n", min(weather_data$DATE), max(weather_data$DATE)))
cat(sprintf("Maximum Temperature recorded: %.1f°F (%.1f°C) on %s\n", 
            max(weather_data$TMAX_F), max(weather_data$TMAX_C), 
            weather_data$DATE[which.max(weather_data$TMAX_F)]))

# -------------------------------------------------------------------------
# 4. Define Premium Theme for Plotting
# -------------------------------------------------------------------------
# A refined, modern ggplot2 theme with a clean grid system and elegant typography.
theme_premium <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      text = element_text(color = "#2b2b2b"),
      plot.title = element_text(size = 18, face = "bold", margin = margin(b = 6), color = "#1a1a1a"),
      plot.subtitle = element_text(size = 12, color = "#555555", margin = margin(b = 20)),
      plot.caption = element_text(size = 9, color = "#777777", margin = margin(t = 15)),
      axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 12)),
      axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 12)),
      axis.text = element_text(size = 10, color = "#444444"),
      panel.grid.major = element_line(color = "#eaeaea", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      legend.position = "right",
      plot.margin = margin(20, 20, 20, 20)
    )
}

# -------------------------------------------------------------------------
# 5. Plot 1: Daily Max Temperatures by Month (Warming Trends Over Years)
# -------------------------------------------------------------------------
# x-axis = Month (Jan-Dec)
# y-axis = Temperature (°F)
# Color = Year (to highlight the warming trend over time)
# For this dense dataset, we use geom_quasirandom to produce a clean swarm.
cat("Generating Plot 1 (Monthly Distribution by Year)...\n")

p1 <- ggplot(weather_data, aes(x = Month, y = TMAX_F, color = Year)) +
  # Use quasirandom beeswarm layout for high performance with 15k points
  geom_quasirandom(
    method = "tukey", 
    alpha = 0.4, 
    size = 0.6, 
    width = 0.4
  ) +
  # Custom continuous color scale showing older years in cool colors and recent years in hot colors
  scale_color_viridis_c(
    option = "inferno", 
    begin = 0.1, 
    end = 0.9, 
    name = "Observation Year",
    breaks = seq(1980, 2020, 10)
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "°F"),
    breaks = seq(10, 120, 10),
    sec.axis = sec_axis(~ (. - 32) / 1.8, name = "Daily Maximum Temperature (°C)", labels = label_number(suffix = "°C"))
  ) +
  labs(
    title = "Portland Daily Maximum Temperature Distribution by Month",
    subtitle = "Daily readings (1979-2021) at Portland International Airport (USW00024229).\nColors indicate progression over the decades, highlighting warmer outliers in recent years.",
    x = "Month of the Year",
    y = "Daily Maximum Temperature (°F)",
    caption = "Data Source: NOAA GHCN-Daily | Visualization: R + ggbeeswarm"
  ) +
  theme_premium()

# Save the plot
ggsave("portland_temps_by_month.png", plot = p1, width = 11, height = 7.5, dpi = 300)
cat("Saved: portland_temps_by_month.png\n")

# -------------------------------------------------------------------------
# 6. Plot 2: Daily Max Temperatures by Year (Seasonal Profile & Heatwaves)
# -------------------------------------------------------------------------
# x-axis = Year (1979 to 2021)
# y-axis = Temperature (°F)
# Color = Season (Winter, Spring, Summer, Autumn)
cat("Generating Plot 2 (Annual Beeswarms & Extremes)...\n")

# Identify the extreme outlier to annotate (June 28, 2021 heatwave)
record_day <- weather_data[which.max(weather_data$TMAX_F), ]

p2 <- ggplot(weather_data, aes(x = factor(Year), y = TMAX_F, color = Season)) +
  geom_quasirandom(
    method = "tukey", 
    alpha = 0.5, 
    size = 0.5, 
    width = 0.38
  ) +
  # Professional seasonal color palette
  scale_color_manual(
    values = c(
      "Winter" = "#3182bd",  # Cool Blue
      "Spring" = "#31a354",  # Green
      "Summer" = "#e6550d",  # Hot Orange
      "Autumn" = "#fdae6b"   # Warm Autumnal Amber
    ),
    name = "Meteorological Season"
  ) +
  scale_x_discrete(
    breaks = as.character(seq(1980, 2020, 5))
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "°F"),
    breaks = seq(10, 120, 10),
    sec.axis = sec_axis(~ (. - 32) / 1.8, name = "Daily Maximum Temperature (°C)", labels = label_number(suffix = "°C"))
  ) +
  # Add annotation pointing to the June 28, 2021 heatwave
  annotate(
    "curve",
    x = "2015", y = 113, xend = "2021", yend = 115.8,
    arrow = arrow(length = unit(0.08, "inches")),
    color = "#8b0000", linewidth = 0.6, curvature = -0.25
  ) +
  annotate(
    "text",
    x = "2010", y = 112,
    label = sprintf("June 28, 2021\nHistoric Record: %.1f°F (%.1f°C)", record_day$TMAX_F, record_day$TMAX_C),
    color = "#8b0000", size = 3.5, fontface = "bold", hjust = 0.5
  ) +
  labs(
    title = "Portland Daily Maximum Temperature Trends (1979-2021)",
    subtitle = "Annual beeswarm distributions side-by-side. Individual days colored by meteorological season.\nNotice the unprecedented 116°F heatwave in late June 2021, towering far above historical bounds.",
    x = "Year of Record",
    y = "Daily Maximum Temperature (°F)",
    caption = "Data Source: NOAA GHCN-Daily | Visualization: R + ggbeeswarm"
  ) +
  theme_premium() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  )

# Save the plot
ggsave("portland_temps_by_year.png", plot = p2, width = 13, height = 7.5, dpi = 300)
cat("Saved: portland_temps_by_year.png\n")

cat("Plot generation complete. Enjoy the visualizations!\n")
