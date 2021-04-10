# Cold pool area analyses
# Sean Rohan <sean.rohan@noaa.gov>

# Load libraries, functions, and themes
source(here::here("R", "1_libraries_functions_themes.R"))

# Leave one out cross-validation to compare interpolation methods ----
source(here::here("R", "loocv_gear_temp.R"))

# Calculate cold pool area using interpolation methods ----
interpolate_gear_temp(proj_crs = "EPSG:3338",
                      cell_resolution = 5000)

# Make plots and tables showing differences among methods (in progress, cpa_change.R) ----
# cpa_change.R