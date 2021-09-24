# Evaluate methods for interpolating EBS bottom temperature using leave-one-out cross validation
# Sean Rohan <sean.rohan@@noaa.gov>
# Last update: September 24, 2021

# Leave-one-out cross-validation of candidate interpolation methods

<<<<<<< HEAD
loocv_gear_temp <- function(temp_data_path, proj_crs) {
  temperature_df <- read.csv(file = temp_data_path,
                             stringsAsFactors = FALSE)
  
  # Columns to lowercase because I could have done a better job designing loocv_2 ----
  names(temperature_df) <- tolower(names(temperature_df))
  
  # Vector of years ----
  year_vec <- sort(unique(temperature_df$year))
  
  # Leave-one-out cross validation for each year ----
  # Sean Rohan's notes: 
  #  - loocv_2() doesn't automatically model anisotropy. A redesign of loocv_2 using functions in geoR instead of gstat may allow a way to automatically estimate anisotropy, but may be time-consuming for more complicated cross-validation.
  #  - Transformations (normal score, Box-Cox) may improve precision. Can investigate further if warranted.
  #  - Other interpolation methods may prove more accurate.
  #  - I question whether precision and accuracy of temperature interpolation provides a reliable measure of the precision and accuracy of cold pool extent.
  #  - For propagating uncertainty, open-source 2D empirical Bayesian kriging may be better... TBD.
  
  print("Starting cross validation using loocv_2()")
  for(i in 1:length(year_vec)) {
    gear_temp_raster <- loocv_2(dat = filter(temperature_df, year == year_vec[i]),
                                             in.proj = "+proj=longlat",
                                             interp.proj = proj_crs
                                lon.col = "longitude",
                                             lat.col = "latitude",
                                             var.col = "gear_temperature",
                                            
                                             pre = paste0("_gear_temperature_", year_vec[i]),
                                             scale.vars = FALSE,
                                             center = FALSE,
                                             scale = FALSE)
  }
  
  # Generate vector of loocv files with "GEAR_TEMPERATURE" in the title.
  # I'd suggest using a loop here if you only want a subset of years on each plot.
  ifelse(!dir.exists(file.path(here::here("output"))), dir.create(file.path(here::here("output"))), FALSE)
  ifelse(!dir.exists(file.path(here::here("output", "loocv"))), dir.create(file.path(here::here("output", "loocv"))), FALSE)
  ifelse(!dir.exists(file.path(here::here("plots"))), dir.create(file.path(here::here("plots"))), FALSE)
  temp_dir <- dir(here::here("output"), full.names = TRUE)
  temp_dir <- temp_dir[grep("gear_temperature", temp_dir)]
  
  # Prediction error plots ----
  print("Making RSPE violin plots")
  plot_loocv_rmse(sel_paths = temp_dir[1:8],
                  y_lab = expression(RSPE~(degree*C)),
                  sel_var = "GEAR_TEMPERATURE",
                  make_plot = TRUE,
                  by_cruise = FALSE,
                  suffix = "_1",
                  fig_res = 600)
  
  plot_loocv_rmse(sel_paths = temp_dir[9:16],
                  y_lab = expression(RSPE~(degree*C)),
                  sel_var = "GEAR_TEMPERATURE",
                  make_plot = TRUE,
                  by_cruise = FALSE,
                  suffix = "_2",
                  fig_res = 600)
  
  plot_loocv_rmse(sel_paths = temp_dir[17:24],
                  y_lab = expression(RSPE~(degree*C)),
                  sel_var = "GEAR_TEMPERATURE",
                  make_plot = TRUE,
                  by_cruise = FALSE,
                  suffix = "_3",
                  fig_res = 600)
  
  plot_loocv_rmse(sel_paths = temp_dir[25:32],
                  y_lab = expression(RSPE~(degree*C)),
                  sel_var = "GEAR_TEMPERATURE",
                  make_plot = TRUE,
                  by_cruise = FALSE,
                  suffix = "_4",
                  fig_res = 600)
  
  plot_loocv_rmse(sel_paths = temp_dir[33:38],
                  y_lab = expression(RSPE~(degree*C)),
                  sel_var = "GEAR_TEMPERATURE",
                  make_plot = TRUE,
                  by_cruise = FALSE,
                  suffix = "_5",
                  fig_res = 600)
=======
loocv_gear_temp <- function(temp_data_path) {

temperature_df <- read.csv(file = temp_data_path,
                           stringsAsFactors = FALSE)

# Columns to lowercase because I could have done a better job designing loocv_2 ----
names(temperature_df) <- tolower(names(temperature_df))

# Vector of years ----
year_vec <- sort(unique(temperature_df$year))

# Leave-one-out cross validation for each year ----
# Sean Rohan's notes: 
#  - loocv_2() doesn't automatically model anisotropy. A redesign of loocv_2 using functions in geoR instead of gstat may allow a way to automatically estimate anisotropy, but may be time-consuming for more complicated cross-validation.
#  - Transformations (normal score, Box-Cox) may improve precision. Can investigate further if warranted.
#  - Other interpolation methods may prove more accurate.
#  - I question whether precision and accuracy of temperature interpolation provides a reliable measure of the precision and accuracy of cold pool extent.
#  - For propagating uncertainty, open-source 2D empirical Bayesian kriging may be better... TBD.

print("Starting cross validation using loocv_2()")
for(i in 1:length(year_vec)) {
  gear_temp_raster <- TLUtilities::loocv_2(dat = filter(temperature_df, year == year_vec[i]),
                                           in.proj = "+proj=longlat",
                                          lon.col = "longitude",
                                          lat.col = "latitude",
                                          var.col = "gear_temperature",
                                          pre = paste0("_gear_temperature_", year_vec[i]),
                                          scale.vars = FALSE,
                                          center = FALSE,
                                          scale = FALSE)
}

# Generate vector of loocv files with "GEAR_TEMPERATURE" in the title.
ifelse(!dir.exists(file.path(here::here("output"))), dir.create(file.path(here::here("output"))), FALSE)
ifelse(!dir.exists(file.path(here::here("output", "loocv"))), dir.create(file.path(here::here("output", "loocv"))), FALSE)
ifelse(!dir.exists(file.path(here::here("plots"))), dir.create(file.path(here::here("plots"))), FALSE)
temp_dir <- dir(here::here("output"), full.names = TRUE) # check that this doesn't include prior data pulls
temp_dir <- temp_dir[grep("gear_temperature", temp_dir)]

# Prediction error plots ----
print("Making RSPE violin plots")
for(i in seq(1, length(year_vec), by = 9)){
  plot_loocv_rmse(sel_paths = temp_dir[i:min(i+8, length(year_vec))],
                y_lab = expression(RSPE~(degree*C)),
                sel_var = "GEAR_TEMPERATURE",
                make_plot = TRUE,
                by_cruise = FALSE,
                suffix = paste0("_", match(i, seq(1, length(year_vec), by = 9))),
                fig_res = 600)
  }
>>>>>>> 608609ea0f49c3c5ac54144f5e0b3d1017236a80
}
