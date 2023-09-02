#' Calculate cold pool area from a raster
#' 
#' Calculate area of raster cells less than or equal to a temperature threshold.
#' 
#' @param x A SpatRaster object.
#' @param raster_units Character vector indicating units for x and y dimensions in the raster. Default = "m" for meters.
#' @param temperature_threshold Numeric. Temperature threshold for value.
#' @export

cpa_from_raster <- function(x, 
                            raster_units = "m", 
                            temperature_threshold) {
  if(raster_units == "m") {
    cell_area <- terra::res(x)[1]*terra::res(x)[2]/1e6 
  }
  
  n_cells <- sum(terra::values(x) <= temperature_threshold, 
                 na.rm = TRUE)
  
  total_area <- n_cells * cell_area
  
  return(total_area)
}
