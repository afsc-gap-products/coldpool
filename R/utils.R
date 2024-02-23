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



#' Discrete continuous bar
#'
#' Generate a continuous bar plot using ggplot functions
#'
#' @param breaks Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
#' @param palette Character vector indicating the name of the RColorBrewer palette to use. Alternatively, can pass a vector of colors to the colors argument.
#' @param colors A vector of colors
#' @export

legend_discrete_cbar = function(
  breaks, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
  palette = "Greys", #
  direction = 1, # Flip colors? Can be 1 or -1
  colors = RColorBrewer::brewer.pal(length(breaks) - 1, palette),
  spacing = "natural", # Spacing between labels. Can be "natural" or "constant"
  border_color = NA, # NA = no border color
  legend_title = NULL,
  legend_direction = "horizontal", # Can be "horizontal" or "vertical"
  font_size = 5,
  expand_size.x = 1,
  expand_size.y = 1,# Controls spacing around legend plot
  expand.y = 1,
  expand.x = 1,
  spacing_scaling = 1, # Multiplicative factor for label and legend title spacing
  width = 0.1, # Thickness of color bar
  triangle_size = 0.1, # Relative width of +-Inf triangles
  title_pos = NULL,
  text.angle = NULL,
  text.vjust = NULL,
  text.hjust = NULL,
  text.color = "black",
  neat.labels = FALSE,
  font.family = "serif"
) {
  require(ggplot2)
  if (!(spacing %in% c("natural", "constant"))) stop("spacing must be either 'natural' or 'constant'")
  if (!(direction %in% c(1, -1))) stop("direction must be either 1 or -1")
  if (!(legend_direction %in% c("horizontal", "vertical"))) stop("legend_direction must be either 'horizontal' or 'vertical'")
  breaks = as.numeric(breaks)
  new_breaks = sort(unique(breaks))
  if (any(new_breaks != breaks)) warning("Wrong order or duplicated breaks")
  breaks = new_breaks
  if (class(colors) == "function") colors = colors(length(breaks) - 1)
  if (length(colors) != length(breaks) - 1) stop("Number of colors (", length(colors), ") must be equal to number of breaks (", length(breaks), ") minus 1")
  if (!missing(colors)) warning("Ignoring RColorBrewer palette '", palette, "', since colors were passed manually")

  if (direction == -1) colors = rev(colors)

  inf_breaks = which(is.infinite(breaks))
  if (length(inf_breaks) != 0) breaks = breaks[-inf_breaks]
  plotcolors = colors

  n_breaks = length(breaks)

  labels = breaks

  if (spacing == "constant") {
    breaks = 1:n_breaks
  }

  r_breaks = range(breaks)

  cbar_df = data.frame(stringsAsFactors = FALSE,
                       y = breaks,
                       yend = c(breaks[-1], NA),
                       color = as.character(1:n_breaks)
  )[-n_breaks,]

  xmin = 1 - width/2
  xmax = 1 + width/2

  cbar_plot = ggplot(cbar_df, aes(xmin=xmin, xmax = xmax, ymin = y * expand.y, ymax = yend * expand.y, fill = factor(color, levels = 1:length(colors)))) +
    geom_rect(show.legend = FALSE,
              color=border_color)

  if (any(inf_breaks == 1)) { # Add < arrow for -Inf
    firstv = breaks[1]
    polystart = data.frame(
      x = c(xmin, xmax, 1),
      y = c(rep(firstv, 2), firstv - diff(r_breaks) * triangle_size)
    )
    plotcolors = plotcolors[-1]
    cbar_plot = cbar_plot +
      geom_polygon(data=polystart, aes(x=x, y=y * expand.y),
                   show.legend = FALSE,
                   inherit.aes = FALSE,
                   fill = colors[1],
                   color=border_color)
  }
  if (any(inf_breaks > 1)) { # Add > arrow for +Inf
    lastv = breaks[n_breaks]
    polyend = data.frame(
      x = c(xmin, xmax, 1),
      y = c(rep(lastv, 2), lastv + diff(r_breaks) * triangle_size)
    )
    plotcolors = plotcolors[-length(plotcolors)]
    cbar_plot = cbar_plot +
      geom_polygon(data=polyend, aes(x=x, y=y * expand.y),
                   show.legend = FALSE,
                   inherit.aes = FALSE,
                   fill = colors[length(colors)],
                   color=border_color)
  }

  if(is.null(text.angle)) {
    text.angle <- 0
  }

  if(is.null(text.hjust)) {
    text.hjust <- 0.5
  }

  if(is.null(text.vjust)) {
    text.vjust <- 0.5
  }

  if (legend_direction == "horizontal") { #horizontal legend
    mul = 1
    x = xmin
    xend = xmax
    cbar_plot = cbar_plot + coord_flip()
    angle = 0
    if(xmax > 0) {
      legend_position = xmax + 0.1 * spacing_scaling
    } else {
      legend_position = xmax + -0.1 * spacing_scaling
    }

  } else { # vertical legend
    mul = -1
    x = xmax
    xend = xmin
    angle = -90
    legend_position = xmax + 0.2 * spacing_scaling
  }

  if(neat.labels) {
    labels <- format(labels, nsmall = 1)
  }

  # print(c(min(x - 0.05 * mul * spacing_scaling), max(x * mul * spacing_scaling)))
  cbar_plot = cbar_plot +
    geom_segment(data = data.frame(y = breaks * expand.y,
                                   yend = breaks * expand.y),
                 aes(y=y,
                     yend=yend),
                 x = x - 0.05 * mul * spacing_scaling,
                 xend = xend,
                 inherit.aes = FALSE,
                 color = text.color) +
    annotate(geom = 'text',
             x = x - 0.1 * mul * spacing_scaling,
             y = breaks * expand.y,
             label = labels,
             angle = text.angle,
             hjust = text.hjust,
             vjust = text.vjust,
             size = font_size,
             color = text.color,
             family = font.family) +
    scale_x_continuous(expand = c(expand_size.x, expand_size.x)) +
    scale_y_continuous(expand = c(expand_size.y, expand_size.y)) +
    scale_fill_manual(values=plotcolors) +
    theme_void()

  if(is.null(title_pos)) {
    title_pos <- mean(r_breaks)
  }

  if (!is.null(legend_title)) { # Add legend title
    cbar_plot = cbar_plot +
      annotate(geom = 'text', x = legend_position, y = title_pos,
               label = legend_title,
               angle = angle,
               size = font_size,
               family = font.family)
  }

  cbar_plot
}



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



#' Custom fermenter fill scale for ggplot2
#'
#' A fermenter scale that accepts custom color palettes.
#'
#' @param pal A character vector of colors to be used for the palette
#' @param na.value Color for NA values
#' @param guide Type of guide for ggplot2::binned_scale to use
#' @param aesthetics ggplot2 aesthetic type for the scale (e.g. "fill")
#' @param ... Addditional arguments passed to ggplot2::binned_scale
#' @export

scale_fill_fermenter_custom <- function(pal, na.value = "grey50", guide = "coloursteps", aesthetics = "fill", ...) {
  ggplot2::binned_scale("fill", "fermenter", ggplot2:::binned_pal(scales::manual_pal(unname(pal))), na.value = na.value, guide = guide, ...)
}



#' Tech memo figure without legend
#' @export

theme_tm_no_legend <- function() {
  theme_bw() %+replace%
    theme(legend.title = element_blank(),
          panel.border = element_rect(color = "black", fill = NA),
          legend.text = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 9, color = "black"),
          axis.text = element_text(size = 8, color = "black"),
          plot.background = element_rect(fill = NA, color = NA))
}



#' Multi-panel map theme with blue strip
#' 
#' Theme for multipanel maps with blue strip
#' 
#' @export

theme_multi_map_blue_strip <- function() {
  theme_bw() %+replace%
    theme(axis.title = element_text(color = "black", face = "bold"),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(size = 9,
                                    color = "white",
                                    face = "bold",
                                    margin = margin(0.5, 0, 0.5, 0, "mm")),
          strip.background = element_rect(fill = "#0055a4",
                                          color = NA))
}



#' Convert decimal degree minutes to decimal degrees
#' 
#' Converts latitude or longitude in decimal degree minutes, DDDMM.MMM (i.e. -17330.000) to decimal degrees, DDD.DDDDD (i.e. -173.50000).
#' 
#' @param x Numerical vector of decimal degree minutes.
#' @return Numerical vector of decimal degrees
#' @export

convert_ddm_to_dd <- function(x) {
  return(floor(x/100) + x%%100/60)
}


