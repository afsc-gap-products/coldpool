#' Interpolate data points to a masked raster
#'
#' This function is a variant of the interpolate_variable function that allows
#' for user-set masking polygons and choice of fitting method.
#'
#' @param dat Input data frame. Must include columns for latitude, longitude, 
#'    and variable to be interpolated.
#' @param dat.year Year
#' @param var.col Character vector denoting the name of the variable column.
#' @param lat.col Character vector indicating the name of the latitude column.
#' @param lon.col Character vector indicating the name of the longitude column.
#' @param in.crs Character vector (PROJ4 or WKT2/PROJ6 string) indicating the 
#'    coordinate reference system for the data.
#' @param interpolation.crs Character vector (PROJ4 or WKT2/PROJ6 string) 
#'    indicating the coordinate reference system to use for the interpolation. 
#'    Historically EPSG:3338
#' @param cell.resolution Dimensions of interpolation grid cell in meters.
#' @param nm Maximum number of cells to use for interpolation.
#' @param select.region Region for interpolation as a character string. Can be 
#'    either a string accepted by akgfmaps::get_base_layers or a simple feature 
#'    polygon
#' @param fittypes vector of strings specifying which fit algorithms to use:
#'    "nn" = nearest neighbor
#'    "idwnmax4" = inverse distance weighting with nmax=4
#'    "idw" = inverse distance weighting
#'    "exp" = ordinary kriging w/ exponential VGM, 
#'    "sph" = ordinary kriging w/ spherical VGM
#'    "bes" = ordinary kriging w/ bessel VGM
#'    "gau" = ordinary kriging w/ gaussian VGM
#'    "cir" = ordinary kriging w/ circular VGM
#'    "mat" = ordinary kriging w/ matern VGM
#'    "ste" = ordinary kriging w/ Stein's matern VGM
#'    "tps" = thin plate spline
#' @param bbox Bounding box for the interpolated grid.  Can be either a string 
#'    ("sebs", "bs.south", "nebs", "ebs", "bs.north", "bs.all") corresponding to 
#'    specific bounds, or a 4-element named vector with values xmin, xmax, ymin, 
#'    and ymax.
#' @return raster stack of interpolated data, with one layer per input fit type

data2raster <- function(dat,
                        dat.year,
                        var.col,
                        lat.col,
                        lon.col,
                        in.crs = "+proj=longlat +datum=NAD83",
                        interpolation.crs,
                        cell.resolution,
                        nm = Inf,
                        select.region = "sebs",
                        fittypes = c("nn", "idwnmax4", "idw", "exp", "sph", "bes", "gau", "cir", "mat", "ste", "tps"),
                        bbox = NULL)
{ 
  
  # Rename data columns for ease
  
  names(dat)[which(names(dat) == var.col)] <- "var.col"
  names(dat)[which(names(dat) == lat.col)] <- "lat.col"
  names(dat)[which(names(dat) == lon.col)] <- "lon.col"
  
  # Remove NAs
  
  if(any(is.na(dat$var.col))) {
    print(paste0("Removing ", 
                 sum(is.na(dat$var.col)), 
                 " var.col NA values from data set"))
    dat <- dat |> 
      dplyr::filter(!is.na(var.col))
  }
  
  # Load AKGF polygon for masking, or the user-supplied mask
  
  mymask <- parseregion(select.region, interpolation.crs)
  
  # Set dimensions for raster cells ---
  # Note: If SEBS or EBS/NEBS is specified, we set the raster to match the older
  # ArcGIS rasters.  If user does not specify anything, a raster is created that
  # encompasses the masking polygon plus a bit of a buffer and aligns with the
  # default SEBS raster.
  
  if (is.character(bbox)) {
    if (bbox %in% c("sebs","bs.south")) {
      bbox = c(xmin = -1625000, 
               xmax = -35000, 
               ymin = 379500, 
               ymax = 1969500)
    } else if (bbox %in% c("nebs","ebs","bs.north","bs.all")) {
      extrap.box <- c(xmn = -179.5, xmx = -157, ymn = 50, ymx = 68)
      plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(extrap.box['xmn'], extrap.box['xmx']), 
                                                                     y = c(extrap.box['ymn'], extrap.box['ymx'])), 
                                                          out.crs = interpolation.crs)
      bbox = c(xmin = plot.boundary$x[1],
               xmax = plot.boundary$x[2],
               ymin = plot.boundary$y[1],
               ymax = plot.boundary$y[2],
      )
    } else {
      stop("Unrecognized value of bbox")
    }
  } else if (is.null(bbox)) {
    bbox <- st_bbox(st_buffer(mymask, cell.resolution*5))
    bbox <- round(bbox/cell_resolution)*cell_resolution # round to align rows/cols
    xshift <- ((-1625000/cell_resolution) - floor(-1625000/cell_resolution))*cell_resolution
    yshift <- ((379500/cell_resolution) - floor(379500/cell_resolution))*cell_resolution
    bbox = bbox + c(xshift,yshift,xshift,yshift)
  }
  
  sp_interp.raster <- raster::raster(xmn = bbox["xmin"],
                                     xmx = bbox["xmax"], 
                                     ymn = bbox["ymin"],
                                     ymx = bbox["ymax"], 
                                     nrows = floor((bbox["ymax"]-bbox["ymin"])/cell.resolution), 
                                     ncols = floor((bbox["xmax"]-bbox["xmin"])/cell.resolution))
  
  raster::projection(sp_interp.raster) <- interpolation.crs
  
  # Transform data for interpolation ----
  sp_interp.df <- unique(dat)
  sp::coordinates(sp_interp.df) <- c(x = "lon.col", y = "lat.col")
  sp::proj4string(sp_interp.df) <- sp::CRS(in.crs)
  sp_interp.df <- sp::spTransform(sp_interp.df, sp::CRS(interpolation.crs))
  
  
  # Fit data and build rasters
  
  # IDW base for variogram-based fits
  
  if (any(c("exp", "sph", "bes", "gau", "cir", "mat", "ste") %in% fittypes)){
    idw_vgm_fit <- gstat::gstat(formula = var.col ~ 1,
                                locations = sp_interp.df,
                                nmax = Inf)
  }
  
  # Loop over requested fit types
  
  outraster <- stack()
  
  for (f in fittypes) {
    
    # Fit data
    
    if (f == "nn") {
      xfit <- gstat::gstat(formula = var.col ~ 1,
                           locations = sp_interp.df,
                           set = list(idp = 0),
                           nmax = 4)
    }
    if (f == "idwnmax4") {
      xfit <- gstat::gstat(formula = var.col ~ 1,
                           locations = sp_interp.df,
                           set = list(idp = 2),
                           nmax = 4)
    }
    if (f == "idw") {
      xfit <- gstat::gstat(formula = var.col ~ 1,
                           locations = sp_interp.df,
                           set = list(idp = 2),
                           nmax = Inf)
    }
    if (f == "exp") {
      vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit),
                                    vgm(c("Exp")))
      
      xfit <- gstat::gstat(formula = var.col ~ 1,
                           locations = sp_interp.df,
                           model = vgfit,
                           nmax = nm)
    }
    if (f == "sph") {
      vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit),
                                    vgm(c("Sph")))
      
      xfit <- gstat::gstat(formula = var.col ~ 1,
                           locations = sp_interp.df,
                           model = vgfit,
                           nmax = nm)
    }
    if (f == "bes") {
      vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit),
                                    vgm(c("Bes")))
      xfit <- gstat::gstat(formula = var.col ~ 1,
                           locations = sp_interp.df,
                           model = vgfit,
                           nmax = nm)
    }
    if (f == "gau") {
      vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit),
                                    vgm(c("Gau")))
      xfit <- gstat::gstat(formula = var.col ~ 1,
                           locations = sp_interp.df,
                           model = vgfit,
                           nmax = nm)
    }
    if (f == "cir") {
      vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit),
                                    vgm(c("Cir")))
      xfit <- gstat::gstat(formula = var.col ~ 1,
                           locations = sp_interp.df,
                           model = vgfit,
                           nmax = nm)
    }
    if (f == "mat") {
      
      vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit),
                                    vgm(c("Mat")))
      xfit <- gstat::gstat(formula = var.col ~ 1,
                           locations = sp_interp.df,
                           model = vgfit,
                           nmax = nm)
    }
    if (f == "ste") {
      
      vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit), 
                                    vgm(c("Ste")))
      
      xfit <- gstat::gstat(formula = var.col ~ 1, 
                           locations = sp_interp.df, 
                           model = vgfit, 
                           nmax = nm)
    }
    if (f == "tps") {
      xfit <- fields::Tps(sp::coordinates(sp_interp.df),
                          sp_interp.df$var.col)
    }
    
    # Build predictor
    
    if (f == "tps") {
      xpredict <- raster::interpolate(sp_interp.raster,
                                      xfit)
    } else {
      xpredict <- predict(xfit, as(sp_interp.raster, "SpatialGrid"))
    }
    
    # Interpolate data to raster and mask outside of polygon
    
    rastmp <- xpredict |>
      akgfmaps::rasterize_and_mask(mymask)
    
    outraster <- addLayer(outraster, rastmp)
  }
  return(outraster)
}



#' Parse region string or simple feature
#' 
#' This helper function parses whether region is provided as a string or as a
#' simple feature. If the former, it loads the appropriate simple feature using
#' the akgfmaps::get_base_layers function; if the latter, it re-projects the
#' provided features to the specified coordinate reference system.
#
#' @param nameorpoly string specifying base layer name, or simple feature object
#' @param crs coordinate reference system to apply to simple feature object
#' @return simple feature object in the requested CRS
#' @export

parseregion <- function(nameorpoly, crs) {
  
  if (is.character(nameorpoly)) {
    region_layers <- akgfmaps::get_base_layers(select.region = nameorpoly, 
                                               set.crs = crs)
    mymask <- region_layers$survey.area
    
  } else if (is.data.frame(nameorpoly)) {
    
    mymask <- sf::st_transform(nameorpoly, crs = sf::st_crs(crs))
  } else {
    stop("region must be an akgfmaps-compatible string or shapefile-derived dataframe")
  }
  
  return(mymask)
  
}



#' Calculate temperature-related indices for the eastern Bering Sea shelf
#'
#' This routine replicates the primary mean temperature and area-below-x indices 
#' as found in the cold_pool_index data frame, but allows for user-specified 
#' variations.
#'
#' @param datafile Full filename of .csv file holding point-sampled surface and 
#'    bottom temperature data
#' @param maskpoly String specifying masking polygon from 
#'    akgfmaps::get_base_layers, or simple feature object to use as masking 
#'    polygon
#' @param select_years vector of years for which indices will be calculated
#' @param cell_resolution resolution (m) to use for interpolation raster
#' @param method fitting method to use for interpolation (see data2raster 
#'    fittype parameter for options)
#' @param proj_crs CRS to use for interpolation, or NULL to use coldpool:::ebs_proj_crs
#' @param threshold vector of temperature threshold values (deg C) for which
#'    area-less-than-x indices will be calculated
#' @param bbox Bounding box to use for interpolated raster (see data2raster)
#' @param bottomvar Name of bottom temperaure variable column in the datafile,
#'    or NULL to skip calculation of bottom temperature metrics
#' @param surfacevar Name of surface temperaure variable column in the datafile,
#'    or NULL to skip calculation of surface temperature metrics
#' @return data frame with the following column
#'    YEAR: year of indices
#'    MEAN_GEAR_TEMPERATURE: mean interpolated and masked bottomvar
#'    MEAN_SURFACE_TEMPERATURE: mean interpolated and masked surfacevar
#'    MEAN_BT_LT100M: mean interpolated bottomvar within the intersection of the 
#'                    <100m survey strata and masked region
#'    AREA_LTE_X: total area (km^2) where temperature is less than X deg C
#' @export

calcindices_temp <- function (datafile, 
                              maskpoly,
                              select_years = NULL,
                              cell_resolution = 5000,
                              method = "ste",
                              proj_crs = NULL, 
                              threshold=c(-1,0,1,2),
                              bbox = NULL, 
                              bottomvar = "gear_temperature",
                              surfacevar = "surface_temperature") 
{
  # Read temperature data from .csv file
  
  print("Reading temperature data...")
  
  temperature_df <- read.csv(file = datafile,
                             stringsAsFactors = FALSE)
  
  # Replace NULL input with defaults
  
  year_vec <- sort(unique(temperature_df$year))
  if(!is.null(select_years)) {
    year_vec <- year_vec[year_vec %in% select_years]
  }
  
  
  if (is.null(proj_crs)) {
    proj_crs <- coldpool:::ebs_proj_crs
  }
  
  # Mask polygons: main and <100m polygon
  
  print("Checking/building masking polygon")
  
  maskpoly <- parseregion(maskpoly, proj_crs)
  
  ebs_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = proj_crs)
  
  lt100_strata <- ebs_layers$survey.strata |>
    dplyr::filter(Stratum %in% c(10, 20, 31, 32, 41, 42, 43)) |>
    dplyr::group_by(SURVEY) |>
    dplyr::summarise()
  
  lt100_strata <- sf::st_intersection(lt100_strata, maskpoly)
  
  # Initialize data frame
  
  print("Initializing data frame")
  
  nyr <- length(select_years)
  nthresh <- length(threshold)
  
  area_lte <- matrix(NA, nyr, nthresh)
  
  bt_df <- data.frame(YEAR = numeric(length = nyr),
                      MEAN_GEAR_TEMPERATURE = numeric(length = nyr),
                      MEAN_BT_LT100M = numeric(length = nyr),
                      MEAN_SURFACE_TEMPERATURE = numeric(length = nyr))
  
  # Build rasters for surface and gear temperature
  
  
  for (i in 1:nyr) {
    
    print(paste("Calculating indices:", select_years[i]))
    
    bt_df$YEAR[i] <- select_years[i]
    
    # Bottom temperature
    
    if (!is.null(bottomvar)) {
      
      ras <- data2raster(dat = dplyr::filter(temperature_df, year == select_years[i]),
                         dat.year = yr,
                         in.crs = "+proj=longlat",
                         interpolation.crs = proj_crs,
                         cell.resolution = cell_resolution,
                         lon.col = "longitude",
                         lat.col = "latitude",
                         var.col = bottomvar,
                         nm = Inf,
                         select.region = maskpoly,
                         fittypes = method, 
                         bbox = bbox)
      ras <- ras[[1]] # 1-layer rasterstack to plain raster
      
      bt_df$MEAN_GEAR_TEMPERATURE[i] <- raster::values(ras) |>
        mean(na.rm = TRUE)
      
      lt100_temp <- raster::mask(ras, lt100_strata)
      bt_df$MEAN_BT_LT100M[i] <- mean(lt100_temp@data@values, na.rm = TRUE)
      
      for (it in 1:nthresh) {
        area_lte[i,it] <- ras |>
          cpa_from_raster(raster_units = "m", temperature_threshold = threshold[it])
      }
      
    }
    
    # Surface temperature
    
    if (!is.null(surfacevar)) {
      ras <- data2raster(dat = dplyr::filter(temperature_df, year == select_years[i]),
                         dat.year = yr,
                         in.crs = "+proj=longlat",
                         interpolation.crs = proj_crs,
                         cell.resolution = cell_resolution,
                         lon.col = "longitude",
                         lat.col = "latitude",
                         var.col = surfacevar,
                         nm = Inf,
                         select.region = maskpoly,
                         fittypes = method,
                         bbox = bbox)
      ras <- ras[[1]] # 1-layer rasterstack to plain raster  
      
      bt_df$MEAN_SURFACE_TEMPERATURE[i] <- raster::values(ras) |>
        mean(na.rm = TRUE)
    }
  }
  area_lte <- as.data.frame(area_lte)
  names(area_lte) <- gsub("-", "m", sprintf("AREA_LTE_%.1f", threshold))
  
  bt_df <- cbind(bt_df, area_lte)
  
  return(bt_df)
  
}