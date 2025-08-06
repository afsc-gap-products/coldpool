#' Save raster to file
#'
#' Saves a raster to file using a custom name for the value column.
#'
#' @param x A raster object
#' @param filename A filename where the raster will be written
#' @param format Format to use to write the raster (see raster::writeRaster)
#' @param overwrite Logical. Should existing rasters be overwritten
#' @param layer_name Character or numeric vector indicating the name for the single layer raster. Must be a valid name for a raster layer.
#' @export

make_raster_file <- function(x,
                             filename,
                             format,
                             overwrite,
                             layer_name) {
  
  names(x) <- layer_name
  
  terra::writeRaster(x,
                     filename = filename,
                     filetype = format,
                     overwrite = overwrite)
  
}



#' Combine multiple raster files into a SpatRaster object with multiple layers
#'
#' Make a raster stack from multiple raster layers that can be opened with  (i.e. raster .grd, GeoTIFF, EHDR)
#' 
#' @param file_path Path to the directory containing raster files
#' @param file_name_contains Character vector to filter on. (e.g., using "ste_" will ignore all filepaths that don't contain the characters sequence)
#' @param file_type File type. Default = ".tif" for GeoTIFF. Also works with native raster .grd and other stackable raster formats.
#' @param wrap Should the SpatRaster be used to create a Packged object (i.e., an object that can be saved as an R object to disk such as .RData or .rds)?
#' @export

make_raster_stack <- function(file_path,
                              file_name_contains = NULL,
                              file_type = ".tif",
                              wrap) {
  
  if(!is.null(file_name_contains)) {
    
    file_paths <- list.files(file_path, full.name = TRUE, pattern = file_name_contains)
    
  } else{
    
    file_paths <- list.files(file_path, full.name = TRUE)
    
  }
  
  file_paths <- file_paths[grepl(pattern = ".tif", x = file_paths)]
  
  rstack <- terra::rast(file_paths[1])
  
  for(ii in 2:length(file_paths)) {
    
    rstack <- c(rstack, terra::rast(file_paths[ii]))
    
  }
  
  if(wrap) {
    rstack <- terra::wrap(rstack)
  }
  
  return(rstack)
}