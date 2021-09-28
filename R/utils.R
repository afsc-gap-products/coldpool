#' Multi-panel map theme
#' @export

theme_multi_map <- function() {
  return(theme_base() %+replace%
           theme(panel.background = element_blank(),
                 panel.border = element_rect(color = "black", fill = NA),
                 panel.grid = element_blank(),
                 plot.title = element_text(size = 9, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
                 axis.title = element_blank(),
                 axis.text = element_text(size = 9),
                 legend.position = "none",
                 plot.background = element_rect(fill = NA, color = NA)))
}

#' Retrieve SQL query string from .sql files
#' 
#' This function extracts a query string from simple .sql file. The SQL file should only contain a comment block at the top and the SQL query itself.
#' 
#' @param sql_path File path to .sql file as a character vector.
#' @return Returns an SQL statement as a character vector, which can be executed on a database connection using functions in the RODBC or ROracle packages.
#' @export

sql_to_rqry <- function(sql_path) {
  in_string <- readr::read_file(sql_path)
  in_string <- sub("/*.*/", "", in_string)
  out_string <- stringr::str_replace_all(in_string, pattern = "\r\n", replacement = " ")
  
  return(out_string)
}


#' Create a database connection using RODBC
#' 
#' A function that accepts a data source name, username, and password to establish returns an Oracle DataBase Connection (ODBC) as an RODBC class in R.
#' 
#' @param schema Data source name (DSN) as a character vector.
#' @return An RODBC class ODBC connection. 
#' @export

get_connected <- function(schema = NA){
  (echo = FALSE)
  if(is.na(schema)) {
    schema <- getPass::getPass(msg = "Enter ORACLE schema: ")
  }
  username <- getPass::getPass(msg = "Enter your ORACLE Username: ")
  password <- getPass::getPass(msg = "Enter your ORACLE Password: ")
  channel  <- RODBC::odbcConnect(dsn = paste(schema),
                                 uid = paste(username),
                                 pwd = paste(password),
                                 believeNRows = FALSE)
}

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
  
  raster::writeRaster(x, 
                      filename = filename,
                      format = format, 
                      overwrite = overwrite)
  
}

#' Make a raster stack from multiple rasters
#' 
#' Make a raster stack from multiple stackable raster layers (i.e. raster .grd, GeoTIFF, EHDR)
#' @param file_path Path to the directory containing raster files
#' @param file_name_contains Character vector to filter on. (e.g., using "ste_" will ignore all filepaths that don't contain the characters sequence)
#' @param file_type File type. Default = ".tif" for GeoTIFF. Also works with native raster .grd and other stackable raster formats.
#' @export

make_raster_stack <- function(file_path,
                              file_name_contains = NULL,
                              file_type = ".tif") {
  
  file_paths <- dir(file_path, full.names = T)[grep(file_type, dir(file_path, full.names = T))]
  
  if(length(file_paths[-grep(".xml", file_paths)]) > 0) {
    file_paths <- file_paths[-grep(".xml", file_paths)]
  }
  
  if(!is.null(file_name_contains)) {
    file_paths <- file_paths[grep(file_name_contains, file_paths)]
  }
  
  for(i in 1:length(file_paths)) {
    if(i == 1) {
      rstack <- raster::stack(raster::raster(rgdal::readGDAL(file_paths[i])))
    } else {
      rstack <- raster::addLayer(rstack, raster(rgdal::readGDAL(file_paths[i])))
    }
  }
  
  return(rstack)
}