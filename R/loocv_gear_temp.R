# Evaluate methods for interpolating EBS bottom temperature using leave-one-out cross validation
# Sean Rohan <sean.rohan@@noaa.gov>
# Last update:April 7, 2021

# Make ODBC connection ---

loocv_gear_temp <- function() {
  
channel <- get_connected()

temperature_df <- sqlQuery(channel, "select gear_temperature,
         start_latitude,
         start_longitude,
         end_latitude,
         end_longitude,
         stationid,
         stratum,
         cruise
         from racebase.haul where
         region = 'BS' and
         (stratum in (10,20,31,32,41,42,43,50,61,62,82,90)) and 
         bottom_depth < 201 and
         (haul_type in (3,13))") %>%
  dplyr::mutate(LATITUDE = (START_LATITUDE + END_LATITUDE)/2,
                LONGITUDE = (START_LONGITUDE + END_LONGITUDE)/2,
                YEAR = floor(CRUISE/100)) %>%
  dplyr::select(-START_LATITUDE, 
                -END_LATITUDE, 
                -START_LONGITUDE, 
                -END_LONGITUDE) %>%
  dplyr::filter(!is.na(GEAR_TEMPERATURE), !is.na(LATITUDE), !is.na(LONGITUDE))

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
                                          pre = paste0("_GEAR_TEMPERATURE_", year_vec[i]),
                                          scale.vars = FALSE,
                                          center = FALSE,
                                          scale = FALSE)
}

# Generate vector of loocv files with "GEAR_TEMPERATURE" in the title.
# I'd suggest using a loop here if you only want a subset of years on each plot.
temp_dir <- dir(here::here("output"), full.names = TRUE)
temp_dir <- temp_dir[grep("GEAR_TEMPERATURE", temp_dir)]

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
}
