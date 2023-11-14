library(coldpool)
library(akgfmaps)
library(gapctd)
library(navmaps)
library(lubridate)

# Get data
source(here::here("R", "get_data.R"))

# 2D LOOCV with temperature
source(here::here("R", "01_interpolate_temperature_2d.R"))

# Plot 2D LOOCV results
source(here::here("R", "02_plot_2d_cross_validation_results.R"))

temperature_2d_cv(region = "GOA")
temperature_2d_cv(region = "EBS")
temperature_2d_cv(region = "AI")