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
#' @param select.region Region for interpolation as a character string. Options = "ebs", "sebs", "nbs"
#' @param methods To use as a character vector. Valid choices: "NN", "IDW", "IDW4", "Exp", "Sph", "Bes", "Gau", "Cir", "Mat", "Ste", "Tps"
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
                                 methods = c("NN", "IDW", "IDW4", "Exp", "Sph", "Bes", "Gau", "Cir", "Mat", "Ste", "Tps"))
{
  
  # dat = dplyr::filter(temperature_df, year == year_vec[ii])
  # dat.year = year_vec[ii]
  # in.crs = "EPSG:4326"
  # interpolation.crs = proj_crs
  # cell.resolution = cell_resolution
  # lon.col = "longitude"
  # lat.col = "latitude"
  # var.col = interp_variable
  # nm = Inf
  # pre = paste0("_", toupper(interp_variable), "_", year_vec[ii])
  # select.region = select_region
  # methods = methods
  
  if(!all(tolower(methods) %in% c("nn", "idw", "idw4", "exp",  "sph", "bes", "gau", "cir", "mat",  "ste", "tps" ))) {
  
    invalid_methods <- methods[which(!(tolower(methods) %in% c("nn", "idw", "idw4", "exp",  "sph", "bes", "gau", "cir", "mat",  "ste", "tps" )))]

    stop(paste0("interpolate_variable: Provided methods ", paste(invalid_method, collapse = ", "), " invalid. See documentation for valid methods (?interpolate_variable"))
      
  }
  
  if(!dir.exists(here::here("output", "raster", select.region, var.col))) {
    dir.create(here::here("output", "raster", select.region, var.col), recursive = TRUE)
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
  
  # Load EBS survey exent for masking ----
  region_layers <- akgfmaps::get_base_layers(select.region = select.region, 
                                           set.crs = interpolation.crs)
  
  # Set dimensions for raster cells ----
  
  if(select.region %in% c("sebs", "bs.south")) {
  # Make raster for interpolation ----
    n_dim <- floor(abs(-1625000 - -35000))/cell.resolution
    
    interp_raster <- terra::rast(xmin = -1625000, 
                                 xmax = -35000, 
                                 ymin = 379500, 
                                 ymax = 1969500, 
                                 nrow = n_dim, 
                                 ncol = n_dim,
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
                                 nrow = n_dim, 
                                 ncol = n_dim,
                                 crs = interpolation.crs)
  
  }

  loc_df <- suppressWarnings(terra::crds(interp_raster, df = TRUE, na.rm = FALSE)) |>
    sf::st_as_sf(coords = c("x", "y"),
                 crs = interpolation.crs)
  
  # Transform data for interpolation ----
  interp_df <- unique(dat) |> # Only unique columns
    sf::st_as_sf(coords = c("lon.col", "lat.col"), crs = in.crs) |>
    sf::st_transform(crs = interpolation.crs)
  
  
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

    # Write masked raster
    akgfmaps::rasterize_and_mask(sgrid = fit, 
                                 amask = region_layers$survey.area) |>
    coldpool::make_raster_file(filename = here::here("output", 
                                                     "raster", 
                                                     select.region,
                                                     var.col, 
                                                     paste0(select.region, "_", tolower(methods[ii]), "_", dat.year, "_", var.col, ".tif" )),
                               format = "GTiff",
                               overwrite = TRUE,
                               layer_name = dat.year)
     
  }
  
}