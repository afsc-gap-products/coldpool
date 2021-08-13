# Cold pool area analyses
# Sean Rohan <sean.rohan@noaa.gov>

# Load libraries, functions, and themes
source(here::here("R", "1_libraries_functions_themes.R"))

# Connect RODBC ----
channel <- get_connected()

# Get temperature data and write csvs to data directory ----
# Writes: 
# -- /data/[date]_all_temperature_data.csv: Temperature data from all hauls
# -- /data/[date]_index_hauls_temperature_data.csv: Temperature dat for haul types 3 and 13
get_data(channel = channel)

# Leave one out cross-validation to compare interpolation methods ----
# Writes:
# -- /plots/RSPE_violin_GEAR_TEMPERATURE_[n].png: Plots of root square prediction error for interpolation methods.
# -- /output/[date]_rmse_loocv_GEAR_TEMPERATURE_[year].csv: Results of leave-one-out cross validation.
loocv_gear_temp(temp_data_path = here::here("data", list.files(here::here("data"))[2]))

# Calculate cold pool area using interpolation methods ----
# Writes:
# -- /output/raster/[method]_[year]_gear_temperature.tif: GeoTIFF raster files for each interpolation method and year
# -- /output/estimate_cpa/cpa_out.csv: Cold pool areas by year for each interpolation method.
interpolate_gear_temp(temp_data_path = here::here("data", list.files(here::here("data"))[2]),
                      proj_crs = "EPSG:3338",
                      cell_resolution = 5000)

# Make plots and tables showing differences among methods (in progress, cpa_change.R) ----
# cpa_change.R
compare_cpa_station_filter(sel_year = 2000,
                           sel_old_raster = "./data/idw_files/bt00_idw/",
                           sel_idw4_geotiff = "./output/raster/idw_nmax4_2000_gear_temperature.tif",
                           set_crs = "EPSG:3338",
                           temp_data_path = here::here("data", list.files(here::here("data"))[2]))
