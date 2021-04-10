#' Interpolate gear temperature to calculate cold pool area using multiple interpolation methods and output rasters of temperature and estimates of Cold Pool Area.
#'
#' @param proj_crs CRS string to use for interpolation as a character vector (CRS should have units of meters)
#' @param cell_resolution Interpolation grid cell dimension in meters.

interpolate_gear_temp <- function(proj_crs, 
                                  cell_resolution) {
  
  # Make ODBC connection ---
  print("Connecting")
  channel <- get_connected()
  temperature_df <- sqlQuery(channel, "select gear_temperature,
         start_latitude,
         start_longitude,
         end_latitude,
         end_longitude,
         stationid,
         stratum,
         haul_type,
         performance,
         cruise
         from racebase.haul where
         region = 'BS' and
         (stratum in (10,20,31,32,41,42,43,50,61,62,82,90)) and 
         cruise > 198200 and
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
  
  names(temperature_df) <- tolower(names(temperature_df))
  
  # Vector of years ----
  year_vec <- sort(unique(temperature_df$year))
  
  print("Calculating cold pool area and making rasters")
  for(i in 1:length(year_vec)) {
    
    cpa_year <- akgfmaps::calculate_cold_pool_area(dplyr::filter(temperature_df, year == year_vec[i]),
                                                   dat.year = year_vec[i],
                                                   in.crs = "+proj=longlat",
                                                   interpolation.crs = proj_crs,
                                                   cell.resolution = cell_resolution,
                                                   lon.col = "longitude",
                                                   lat.col = "latitude",
                                                   var.col = "gear_temperature",
                                                   nm = Inf,
                                                   pre = paste0("_GEAR_TEMPERATURE_", year_vec[i]),
                                                   write.to.file = TRUE)
    print(cpa_year)
    
    if(i == 1) {
      cpa_out <- cpa_year
    } else {
      cpa_out <- dplyr::bind_rows(cpa_out, cpa_year)
    }
    
  }
  

  # Write cold pool area calculations to csv ----
  print("Checking for output directory")
  if(!dir.exists(here::here("output", "estimated_cpa"))) {
    dir.create(here::here("output", "estimated_cpa"))  
  }
  
  print("Writing cold pool area to output")
  write.csv(cpa_out, 
            file = here::here("output", "estimated_cpa", "cpa_out.csv"), 
            row.names = FALSE)
  
}
