#' Make csv table of variables for each method
#' 
#' This function uses rasters to generate estimates of area by temperature variables, mean gear tempearture, and mean surface temperature.
#' 
#' @param method_prefix File name prefixes for rasters with methods to be compared.
#' @param temp_dir Path to directory where rasters are saved (not to gear_temperature and surface_temperature subdirectories; the directory that contains gear_temperature and surface_temperature subdirectories)
#' @param region Add a region prefix to the output csv files.
#' @noRd

make_var_by_method <- function(method_prefix = c("ste_", "tps_", "mat_", "sph_", "cir_", "gau_", "bes_", "idw_", "exp_", "idwnmax4_", "nn_"), 
                               temp_dir = here::here("output", "raster", "sebs"), 
                               region = "sebs") {
  
  bottom_temp_dir <- here::here(temp_dir, "gear_temperature")
  surface_temp_dir <- here::here(temp_dir, "surface_temperature")
  
  for(jj in 1:length(method_prefix)) {
    
    surface_temp_files <- list.files(surface_temp_dir, full.names = TRUE)
    surface_temp_files <- surface_temp_files[grep(pattern = method_prefix[jj], x = surface_temp_files)]
    
    if(!(length(surface_temp_files) >= 1)) {
      stop(paste0("No surfacetemperature rasters found for method ",  method_prefix[jj]))
    }
    
    bottom_temp_files <- list.files(bottom_temp_dir, full.names = TRUE)
    bottom_temp_files <- bottom_temp_files[grep(pattern = method_prefix[jj], x = bottom_temp_files)]
    
    if(!(length(bottom_temp_files) >= 1)) {
      stop(paste0("No bottom temperature rasters found for method ",  method_prefix[jj]))
    }
    
    bt_df <- data.frame(YEAR = numeric(length = length(bottom_temp_files)),
                        AREA_LTE2_KM2 = numeric(length = length(bottom_temp_files)),
                        AREA_LTE1_KM2 = numeric(length = length(bottom_temp_files)),
                        AREA_LTE0_KM2 = numeric(length = length(bottom_temp_files)),
                        AREA_LTEMINUS1_KM2 = numeric(length = length(bottom_temp_files)),
                        MEAN_GEAR_TEMPERATURE = numeric(length = length(bottom_temp_files)))
    
    for(ii in 1:length(bottom_temp_files)) {
      bt_raster <- raster::raster(bottom_temp_files[ii], values = TRUE)
      bt_df$YEAR[ii] <- as.numeric(gsub("[^0-9.-]", "", gsub(method_prefix[jj], "", names(bt_raster)))) # Extract year
      bt_df$AREA_LTE2_KM2[ii] <- bt_raster %>% 
        cpa_from_raster(raster_units = "m", temperature_threshold = 2)
      bt_df$AREA_LTE1_KM2[ii] <- bt_raster %>% 
        cpa_from_raster(raster_units = "m", temperature_threshold = 1)
      bt_df$AREA_LTE0_KM2[ii] <- bt_raster %>% 
        cpa_from_raster(raster_units = "m", temperature_threshold = 0)
      bt_df$AREA_LTEMINUS1_KM2[ii] <- bt_raster %>% 
        cpa_from_raster(raster_units = "m", temperature_threshold = -1)
      bt_df$MEAN_GEAR_TEMPERATURE[ii] <- raster::values(bt_raster) %>% 
        mean(na.rm = TRUE)
      
    }
    
    # Calculate mean surface temperature
    
    sst_df <- data.frame(YEAR = numeric(length = length(surface_temp_files)),
                         MEAN_SURFACE_TEMPERATURE = numeric(length = length(surface_temp_files)))
    
    for(ii in 1:length(surface_temp_files)) {
      sst_raster <- raster::raster(surface_temp_files[ii], values = TRUE)
      sst_df$YEAR[ii] <- as.numeric(gsub("[^0-9.-]", "", gsub(method_prefix[jj], "", names(sst_raster))))  # Extract year
      sst_df$MEAN_SURFACE_TEMPERATURE[ii] <- raster::values(sst_raster) %>% 
        mean(na.rm = TRUE)
    }
    
    bt_sst_df <- dplyr::inner_join(bt_df, sst_df, by = c("YEAR"))
    names(bt_sst_df) <- tolower(names(bt_sst_df))
    names(bt_sst_df)[names(bt_sst_df) != "year"] <- paste0(method_prefix[jj], names(bt_sst_df)[names(bt_sst_df) != "year"])
    
    print(bt_sst_df)
    
    if(jj == 1) {
      output_df <- bt_sst_df
    } else {
      output_df <- dplyr::inner_join(output_df, bt_sst_df)
    }
    
  }
  
  write.csv(output_df, 
            file = here::here("output", paste0(region, "_variable_est_by_method.csv")), 
            row.names = FALSE)
}
