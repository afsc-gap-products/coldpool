#' Interpolate gear temperature to calculate cold pool area using multiple interpolation methods and output rasters of temperature and estimates of Cold Pool Area.
#'
#' @param proj_crs CRS string to use for interpolation as a character vector (CRS should have units of meters)
#' @param cell_resolution Interpolation grid cell dimension in meters.

interpolate_gear_temp <- function(temp_data_path,
                                  proj_crs, 
                                  cell_resolution) {
  temp_data_path <- here::here("data", "2021-09-23_index_hauls_temperature_data.csv")
  temperature_df <- read.csv(file = temp_data_path,
                             stringsAsFactors = FALSE)
  temperature_df <- dplyr::filter(temperature_df, cruise == 201601)
  names(temperature_df) <- tolower(names(temperature_df))
  
  # Vector of years ----
  year_vec <- sort(unique(temperature_df$year))
  
  print("Calculating cold pool area and making rasters")
  for(i in 1:length(year_vec)) {
    
    cpa_year <- calculate_cold_pool_area(dat = dplyr::filter(temperature_df, year == year_vec[i]),
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
  
  cpa_out_ste <- dplyr::select(cpa_out, year, ste_lte2) # cold pool area for only best method

  # Write cold pool area calculations to csv ----
  print("Checking for output directory")
  if(!dir.exists(here::here("output", "estimated_cpa"))) {
    dir.create(here::here("output", "estimated_cpa"))  
  }
  
  print("Writing cold pool area to output")
  write.csv(cpa_out, 
            file = here::here("output", "estimated_cpa", "cpa_out.csv"), 
            row.names = FALSE)
  write.csv(cpa_out_ste, 
            file = here::here("output", "estimated_cpa", "cpa_out_ste.csv"), 
            row.names = FALSE)
  
}
