library(coldpool)
library(gapctd)
library(navmaps)
library(lubridate)

# Retrieve temperature/depth profiles, low-pass filter temperature data, and calculate average temperature for 1-m depth bins.
# Outputs: data/[region]/profile_data_[region].rds
source(here::here("R", "01_get_data.R"))

# Thin profile data to 5-m depth intervals, exclude surface measurements (<5 m), and assign each haul to a cross-validation fold. EBS, NBS, and slope data are combined into a single region (EBS)
# Outputs: data/[region]/cv_data_[region].rds
source(here::here("R", "02_setup_crossvalidation_data.R"))

# Conduct 10-fold cross-validation on temperature interpolation using ordinary kriging, regression kriging, nearest neighbor, and inverse distance weighting interpolation.
# Outputs: output/[region]/2d_interp_cv_temperature_[region].rds
source(here::here("R", "03_interpolate_temperature_2d.R"))

# Make tables and plots of 2D cross validation results
source(here::here("R", "04_plot_2d_cross_validation_results.R"))
