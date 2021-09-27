# Include cold pool index package 
# Run after interpolating temperature and calculating cold pool area.
library(coldpool)

# CRS
proj_crs <- coldpool:::ebs_proj_crs

# Connect RODBC ----
channel <- get_connected()

# Get temperature data and write csvs to data directory ----
# Writes: 
# -- /data/[date]_all_temperature_data.csv: Temperature data from all hauls
# -- /data/[date]_index_hauls_temperature_data.csv: Temperature data for haul types 3 and 13
get_data(channel = channel)

# Leave one out cross-validation to compare interpolation methods ----
# Writes:
# -- /plots/RSPE_violin_GEAR_TEMPERATURE_[n].png: Plots of root square prediction error for interpolation methods.
# -- /output/[date]_rmse_loocv_GEAR_TEMPERATURE_[year].csv: Results of leave-one-out cross validation.
# loocv_gear_temp(temp_data_path =  here::here("data", "2021-09-24_index_hauls_temperature_data.csv"),
#                 proj_crs = proj_crs) # update file name manually if need specific data file date

# Calculate cold pool area using interpolation methods ----
# Writes:
# -- /output/raster/[method]_[year]_gear_temperature.tif: GeoTIFF raster files for each interpolation method and year
# -- /output/estimate_cpa/cpa_out.csv: Cold pool areas by year for each interpolation method.
interpolate_gear_temp(temp_data_path =  here::here("data", "2021-09-24_index_hauls_temperature_data.csv"),
                      proj_crs = proj_crs,
                      cell_resolution = 5000)

cpa_pre2021 <- read.csv(file = "./inst/extdata/cpa_areas2019.csv")
ebs_proj_crs <- "EPSG:3338"
cold_pool_index <- read.csv(file = "./output/estimated_cpa/cpa_out_ste.csv") %>%
  dplyr::rename(COLD_POOL_AREA_KM2 = ste_lte2, YEAR = year) %>%
  dplyr::mutate(LAST_UPDATE = Sys.Date())
save(cpa_pre2021, ebs_proj_crs, cold_pool_index, file = "./R/sysdata.rda")