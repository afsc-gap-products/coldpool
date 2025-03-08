#' Interpolate variable, mask to extent, write raters to file.
#' 
#' This function runs interpolations, raster masking using akgfmaps::rasterize_and_mask(), and coldpool::cpa_from_raster to calculate cold pool area using 
#' 
#' @param dat Input data frame. Must include columns for latitude, longitude, and variable to be interpolated.
#' @param dat.year Year
#' @param var.col Character vector denoting the name of the variable column.
#' @param lat.col Character vector indicating the name of the latitude column.
#' @param lon.col Character vector indicating the name of the longitude column.
#' @param in.crs Character vector (PROJ4 or WKT2/PROJ6 string) indicating the coordinate reference system for the data.
#' @param interpolation.crs Character vector (PROJ4 or WKT2/PROJ6 string) indicating the coordinate reference system to use for the interpolation. Historically EPSG:3338
#' @param cell.resolution Dimensions of interpolation grid cell in meters.
#' @param nm Maximum number of cells to use for interpolation. 
#' @param pre Prefix for file names in output (in development.)
#' @param select.region Region for interpolation as a character string OR an sf object. Character options = "ebs", "sebs", "nbs". If select.region is an sf object, must provide bbox.
#' @param methods To use as a character vector. Valid choices: "NN", "IDW", "IDW4", "Exp", "Sph", "Bes", "Gau", "Cir", "Mat", "Ste", "Tps"
#' @param bbox Bounding box for the interpolated grid.  Can be either a string 
#'    ("sebs", "bs.south", "nebs", "ebs", "bs.north", "bs.all") corresponding to 
#'    specific bounds, or a 4-element named vector with values xmin, xmax, ymin, 
#'    and ymax.
#' @param return.raster Should a raster be returned?
#' @param outputfilefun Function of the form f(mask, year, method, var) that returns a string holding the full path name where the output raster should be stored.  If not included, default is [coldpool_dir]/output/raster/[mask]/[var]/[mask]_[method]_[year]_[var].tif
#' @return Returns a data frame containing cold pool areas estimated by interpolation methods, for cutoffs at zero degrees and two degrees C. If argument write.to.file = TRUE, also writes a GeoTIFF raster to output directory.
#' @export

interpolate_variable <- function(dat,
                                 dat.year,
                                 var.col,
                                 lat.col,
                                 lon.col,
                                 in.crs = "EPSG:4326",
                                 interpolation.crs,
                                 cell.resolution,
                                 interpolation.extent = NULL,
                                 nm = Inf,
                                 pre = NA,
                                 select.region = "sebs",
                                 methods = c("NN", "IDW", "IDW4", "Exp", "Sph", "Bes", "Gau", "Cir", "Mat", "Ste", "Tps"),
                                 bbox = NULL,
                                 return_raster = FALSE,
                                 outputfilefun = NULL) {
  
  #---------------------
  # Input checks
  #---------------------
  
  if(!all(tolower(methods) %in% c("nn", "idw", "idw4", "exp",  "sph", "bes", "gau", "cir", "mat",  "ste", "tps" ))) {
  
    invalid_methods <- methods[which(!(tolower(methods) %in% c("nn", "idw", "idw4", "exp",  "sph", "bes", "gau", "cir", "mat",  "ste", "tps" )))]

    stop(paste0("interpolate_variable: Provided methods ", paste(invalid_method, collapse = ", "), " invalid. See documentation for valid methods (?interpolate_variable"))
      
  }
  
  if(return_raster) {
    stopifnot("interpolate_variable: Can only choose one method if the raster is returned" = length(methods) == 1)
  }
  
  names(dat)[which(names(dat) == var.col)] <- "var.col"
  names(dat)[which(names(dat) == lat.col)] <- "lat.col"
  names(dat)[which(names(dat) == lon.col)] <- "lon.col"
  
  # Remove NAs
  if(any(is.na(dat$var.col))) {
    print(paste0("coldpool::interpolate_variable: Removing ", 
                 sum(is.na(dat$var.col)), 
                 " var.col NA values from data set"))
    dat <- dat |> 
      dplyr::filter(!is.na(var.col))
  }
  
  
  #---------------------
  # Raster setup
  #---------------------
  
  # When select.region is a character vector
  if(is.character(select.region)) {
    # Load survey extent for masking ----
    region_mask <- akgfmaps::get_base_layers(select.region = select.region, 
                                               set.crs = interpolation.crs)$survey.area
  } 
  
  # When select.region is an sf object
  if(is.data.frame(select.region)) {
    region_mask <- select.region
  }
  
  # Set dimensions for raster cells ----
  
  if(is.null(bbox)) {
    if (is.character(select.region)) { # Default interpolation bounds for named regions
    
      if(select.region %in% c("sebs", "bs.south")) {
        # Make raster for interpolation ----
        n_dim <- floor(abs(-1625000 - -35000))/cell.resolution
      
        interp_raster <- terra::rast(xmin = -1625000, 
                                     xmax = -35000, 
                                     ymin = 379500, 
                                     ymax = 1969500, 
                                     nrow = n_dim[1], 
                                     ncol = n_dim[2],
                                     crs = interpolation.crs)
      
      } else {
      
        if(select.region %in% c("bs.north", "nbs")) {extrap.box = c(xmn = -179.5, xmx = -157, ymn = 50, ymx = 68)}
      
        if(select.region %in% c("bs.all", "ebs")) {extrap.box = c(xmn = -179.5, xmx = -157, ymn = 50, ymx = 68)}
      
        plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(extrap.box['xmn'], extrap.box['xmx']), 
                                                                       y = c(extrap.box['ymn'], extrap.box['ymx'])), 
                                                            out.crs = interpolation.crs)
        
        n_dim <- floor(abs(plot.boundary$x[1] - plot.boundary$x[2]))/cell.resolution
      
        interp_raster <- terra::rast(xmin = plot.boundary$x[1], 
                                     xmax = plot.boundary$x[2], 
                                     ymin = plot.boundary$y[1], 
                                     ymax = plot.boundary$y[2], 
                                     nrow = n_dim[1], 
                                     ncol = n_dim[2],
                                     crs = interpolation.crs)

      }
    } else { # Default interpolation bounds for sf object regions
      bbox <- sf::st_bbox(st_buffer(region_mask, cell.resolution*5))
      bbox <- round(bbox/cell_resolution)*cell_resolution # round to align rows/cols
      xshift <- ((-1625000/cell_resolution) - floor(-1625000/cell_resolution))*cell_resolution
      yshift <- ((379500/cell_resolution) - floor(379500/cell_resolution))*cell_resolution
      bbox <- bbox + c(xshift,yshift,xshift,yshift)
      
      interp_raster <- terra::rast(xmin = bbox["xmin"],
                                   xmax = bbox["xmax"],
                                   ymin = bbox["ymin"],
                                   ymax = bbox["ymax"],
                                   nrows = floor((bbox["ymax"]-bbox["ymin"])/cell.resolution),
                                   ncols = floor((bbox["xmax"]-bbox["xmin"])/cell.resolution),
                                   crs = interpolation.crs)
      
    }
  } else { # User-customized interpolation bounds
    
    interp_raster <- terra::rast(xmin = bbox["xmin"],
                                 xmax = bbox["xmax"],
                                 ymin = bbox["ymin"],
                                 ymax = bbox["ymax"],
                                 nrows = floor((bbox["ymax"]-bbox["ymin"])/cell.resolution),
                                 ncols = floor((bbox["xmax"]-bbox["xmin"])/cell.resolution),
                                 crs = interpolation.crs)
  }
  
  

  loc_df <- suppressWarnings(terra::crds(interp_raster, df = TRUE, na.rm = FALSE)) |>
    sf::st_as_sf(coords = c("x", "y"),
                 crs = interpolation.crs)
  
  # Transform data for interpolation ----
  interp_df <- unique(dat) |> # Only unique columns
    sf::st_as_sf(coords = c("lon.col", "lat.col"), crs = in.crs) |>
    sf::st_transform(crs = interpolation.crs)
  
  
  #---------------------
  # Interpolate
  #---------------------
  
  for(ii in 1:length(methods)) {
    
    if(tolower(methods[ii]) == "nn") {
      mod <- gstat::gstat(formula = var.col ~ 1,
                             locations = interp_df,
                             set = list(idp = 0),
                             nmax = 4)
      fit <- predict(object = mod, newdata = loc_df)
    }
    
    if(tolower(methods[ii]) == "idw4") {
  
      mod <- gstat::gstat(formula = var.col ~ 1,
                          locations = interp_df,
                          set = list(idp = 2),
                          nmax = 4)
      fit <- predict(object = mod, newdata = loc_df)
      
    }
    
    if(tolower(methods[ii]) == "idw") {
      
      mod <- gstat::gstat(formula = var.col ~ 1,
                          locations = interp_df,
                          set = list(idp = 2),
                          nmax = Inf)
      fit <- predict(object = mod, newdata = loc_df)
      
    }
    
    if(tolower(methods[ii]) %in% c("exp", "sph", "bes", "gau", "cir", "mat", "ste")) {
      
      vgm_type <- c("Exp", "Sph", "Bes", "Gau", "Cir", "Mat", "Ste")[match(tolower(methods[ii]), c("exp", "sph", "bes", "gau", "cir", "mat", "ste"))]

      # Set up object for kriging ----
      idw_vgm_fit <- gstat::gstat(formula = var.col ~ 1, 
                                  locations = interp_df, 
                                  nmax = Inf)
      
      # Estimate variogram
      vgfit <- gstat::fit.variogram(object = gstat::variogram(idw_vgm_fit),
                                    model = gstat::vgm(vgm_type))
      
      # Add variogram
      mod <- gstat::gstat(formula = var.col ~ 1,
                          locations = interp_df,
                          model = vgfit,
                          nmax = nm)
      
      fit <- predict(object = mod, newdata = loc_df)
      
    }
    
    
    if(tolower(methods[ii]) == "tps") {
      
      mod <- fields::Tps(x = sf::st_coordinates(interp_df),
                         Y = interp_df$var.col)

      fit <- loc_df
      
      fit$var1.pred <- predict(object = mod, x = sf::st_coordinates(loc_df))
      
    }
    
    if("sf" %in% class(fit)) {
       terra::values(interp_raster) <- fit$var1.pred
       fit <- interp_raster
    }
    
    # Added to maintain backwards compatibility with data2raster
    if(return_raster) {
      return(fit)
    }

    #---------------------
    # Write raster to file
    #---------------------
    
    # Output folder and filename setup
  
    if (is.character(select.region)) {
      maskname <- select.region
    } else if (is.data.frame(select.region)) {
      maskname <- ifelse(is.na(pre), "var", pre)
    }
  
    if (is.null(outputfilefun)) {
      # Default location: [coldpool]/output/raster/[mask]/[var]/[mask]_[method]_[year]_[var].tif
      outfolder <- here::here("output", "raster", maskname, var.col)
      outfile <- paste0(maskname, "_", tolower(methods[ii]), "_", dat.year, "_", var.col, ".tif" )
    } else {
      # User-customized location
      outfile <- outputfilefun(maskname, dat.year, methods[ii], var.col)
      outfolder <- dirname(outfile)
      outfile  <- basename(outfile)
    }
  
    if (!dir.exists(outfolder)) {
      dir.create(outfolder, recursive = TRUE)
    }
    
    # Rasterize and save to geotiff
    
    akgfmaps::rasterize_and_mask(sgrid = fit, 
                                 amask = region_mask) |>
    coldpool::make_raster_file(filename = file.path(outfolder, outfile),
                               format = "GTiff",
                               overwrite = TRUE,
                               layer_name = dat.year)
     
  }
  

  
}
