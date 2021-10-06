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
#' @return Returns a data frame containing cold pool areas estimated by interpolation methods, for cutoffs at zero degrees and two degrees C. If argument write.to.file = TRUE, also writes a GeoTIFF raster to output directory.
#' @export

interpolate_variable <- function(dat,
                                     dat.year,
                                     var.col,
                                     lat.col,
                                     lon.col,
                                     in.crs = "+proj=longlat +datum=NAD83",
                                     interpolation.crs,
                                     cell.resolution,
                                 interpolation.extent = NULL,
                                     nm = Inf,
                                     pre = NA,
                                 select.region = "sebs")
{
  names(dat)[which(names(dat) == var.col)] <- "var.col"
  names(dat)[which(names(dat) == lat.col)] <- "lat.col"
  names(dat)[which(names(dat) == lon.col)] <- "lon.col"
  
  # Remove NAs
  if(any(is.na(dat$var.col))) {
    print(paste0("coldpool::interpolate_variable: Removing ", 
                 sum(is.na(dat$var.col)), 
                 " var.col NA values from data set"))
    dat <- dat %>% 
      dplyr::filter(!is.na(var.col))
  }
  
  # Load EBS survey exent for masking ----
  region_layers <- akgfmaps::get_base_layers(select.region = select.region, 
                                           set.crs = interpolation.crs)
  
  # Set dimensions for raster cells ----

  
  if(select.region %in% c("sebs", "bs.south")) {
  # Make raster for interpolation ----
    n_dim <- floor(abs(-1625000 - -35000))/cell.resolution
  sp_interp.raster <- raster::raster(xmn = -1625000, 
                                     xmx = -35000, 
                                     ymn = 379500, 
                                     ymx = 1969500, 
                                     nrow = n_dim, 
                                     ncol = n_dim)
  } else {
    if(select.region %in% c("bs.north", "nbs")) {extrap.box = c(xmn = -179.5, xmx = -157, ymn = 50, ymx = 68)}
    if(select.region %in% c("bs.all", "ebs")) {extrap.box = c(xmn = -179.5, xmx = -157, ymn = 50, ymx = 68)}

    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(extrap.box['xmn'], extrap.box['xmx']), 
                                                                   y = c(extrap.box['ymn'], extrap.box['ymx'])), 
                                                        out.crs = interpolation.crs)
    

    
    n_dim <- floor(abs(plot.boundary$x[1] - plot.boundary$x[2]))/cell.resolution
    sp_interp.raster <- raster::raster(xmn = plot.boundary$x[1], 
                                       xmx = plot.boundary$x[2], 
                                       ymn = plot.boundary$y[1], 
                                       ymx = plot.boundary$y[2], 
                                       nrow = n_dim, 
                                       ncol = n_dim)
  }
  
  raster::projection(sp_interp.raster) <- interpolation.crs
  
  # Transform data for interpolation ----
  sp_interp.df <- unique(dat)
  sp::coordinates(sp_interp.df) <- c(x = "lon.col", y = "lat.col")
  sp::proj4string(sp_interp.df) <- sp::CRS(in.crs)
  sp_interp.df <- sp::spTransform(sp_interp.df, sp::CRS(interpolation.crs))
  
  # Nearest-neighbor
  nn_fit <- gstat::gstat(formula = var.col ~ 1,
                         locations = sp_interp.df,
                         set = list(idp = 0),
                         nmax = 4)
  nn.predict <- predict(nn_fit, as(sp_interp.raster, "SpatialGrid"))

  # Inverse distance weighting w/ nmax = 4 (ArcGIS Default) ----
  idw_nmax4_fit <- gstat::gstat(formula = var.col ~ 1,
                                locations = sp_interp.df,
                                set = list(idp = 2),
                                nmax = 4)
  idw_nmax4.predict <- predict(idw_nmax4_fit, as(sp_interp.raster, "SpatialGrid"))

  # Inverse distance weighting w/ nmax = Inf
  idw_fit <- gstat::gstat(formula = var.col ~ 1,
                          locations = sp_interp.df,
                          set = list(idp = 2),
                          nmax = Inf)

  idw.predict <- predict(idw_fit, as(sp_interp.raster, "SpatialGrid"))
  
  # Set up a new IDW for ordinary kriging ----
  idw_vgm_fit <- gstat::gstat(formula = var.col ~ 1, 
                              locations = sp_interp.df, 
                              nmax = Inf)
  
  # Ordinary Kriging: Exponential VGM----
  exp.vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit),
                                    vgm(c("Exp")))
  exp_fit <- gstat(formula = var.col ~ 1,
                   locations = sp_interp.df,
                   model = exp.vgfit,
                   nmax = nm)
  exp.predict <- predict(exp_fit, as(sp_interp.raster, "SpatialGrid"))

  # Ordinary Kriging: Spherical VGM----
  sph.vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit),
                                    vgm(c("Sph")))
  sph_fit <- gstat::gstat(formula = var.col ~ 1,
                          locations = sp_interp.df,
                          model = sph.vgfit,
                          nmax = nm)
  sph.predict <- predict(sph_fit, as(sp_interp.raster, "SpatialGrid"))

  # Ordinary Kriging: Bessel VGM----
  bes.vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit),
                                    vgm(c("Bes")))
  bes_fit <- gstat::gstat(formula = var.col ~ 1,
                          locations = sp_interp.df,
                          model = bes.vgfit,
                          nmax = nm)
  bes.predict <- predict(bes_fit, as(sp_interp.raster, "SpatialGrid"))

  # Ordinary Kriging: Bessel VGM----
  gau.vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit),
                                    vgm(c("Gau")))
  gau_fit <- gstat::gstat(formula = var.col ~ 1,
                          locations = sp_interp.df,
                          model = gau.vgfit,
                          nmax = nm)
  gau.predict <- predict(gau_fit, as(sp_interp.raster, "SpatialGrid"))

  # Ordinary Kriging: Circular VGM----
  cir.vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit),
                                    vgm(c("Cir")))
  cir_fit <- gstat::gstat(formula = var.col ~ 1,
                          locations = sp_interp.df,
                          model = cir.vgfit,
                          nmax = nm)
  cir.predict <- predict(cir_fit, as(sp_interp.raster, "SpatialGrid"))

  # Ordinary Kriging: Matern VGM----
  mat.vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit),
                                    vgm(c("Mat")))
  mat_fit <- gstat::gstat(formula = var.col ~ 1,
                          locations = sp_interp.df,
                          model = mat.vgfit,
                          nmax = nm)
  mat.predict <- predict(mat_fit, as(sp_interp.raster, "SpatialGrid"))
  
  # Ordinary Kriging: Stein's Matern VGM----
  ste.vgfit <- gstat::fit.variogram(variogram(idw_vgm_fit), 
                                    vgm(c("Ste")))
  ste_fit <- gstat::gstat(formula = var.col ~ 1, 
                          locations = sp_interp.df, 
                          model = ste.vgfit, 
                          nmax = nm)
  ste.predict <- predict(ste_fit, as(sp_interp.raster, "SpatialGrid"))

  # Thin-plate spline ----
  tps_fit <- fields::Tps(sp::coordinates(sp_interp.df),
                         sp_interp.df$var.col)
  tps.predict <- raster::interpolate(sp_interp.raster,
                                     tps_fit)
  
  print("Writing rasters")
  if(!dir.exists(here::here("output"))) {
    dir.create(here::here("output"))
  }
  if(!dir.exists(here::here("output", "raster"))) {
    dir.create(here::here("output", "raster"))
  }
  if(!dir.exists(here::here("output", "raster", select.region))) {
    dir.create(here::here("output", "raster", select.region))
  }
  if(!dir.exists(here::here("output", "raster", select.region, var.col))) {
    dir.create(here::here("output", "raster", select.region, var.col))
  }

  
  # write unmasked surfaces to raster
  coldpool::make_raster_file(nn.predict %>%
                               akgfmaps::rasterize_and_mask(region_layers$survey.area),
                             filename = here::here("output", 
                                                   "raster", 
                                                   select.region,
                                                   var.col, paste0(select.region, "_nn_", dat.year, "_", var.col, ".tif" )),
                             format = "GTiff",
                             overwrite = TRUE,
                             layer_name = dat.year)
  coldpool::make_raster_file(idw_nmax4.predict %>%
                               akgfmaps::rasterize_and_mask(region_layers$survey.area),
                             filename = here::here("output", 
                                                   "raster", 
                                                   select.region, 
                                                   var.col, paste0(select.region, "_idw_nmax4_", dat.year, "_", var.col, ".tif" )),
                             format = "GTiff",
                             overwrite = TRUE,
                             layer_name = dat.year)
  coldpool::make_raster_file(idw.predict %>%
                               akgfmaps::rasterize_and_mask(region_layers$survey.area),
                             filename = here::here("output", 
                                                   "raster", 
                                                   select.region, 
                                                   var.col, 
                                                   paste0(select.region, "_idw_", dat.year, "_", var.col, ".tif" )),
                             format = "GTiff",
                             overwrite = TRUE,
                             layer_name = dat.year)
  coldpool::make_raster_file(exp.predict %>%
                               akgfmaps::rasterize_and_mask(region_layers$survey.area),
                             filename = here::here("output", 
                                                   "raster", 
                                                   select.region, 
                                                   var.col, 
                                                   paste0(select.region, "_exp_", dat.year, "_", var.col, ".tif" )),
                             format = "GTiff",
                             overwrite = TRUE,
                             layer_name = dat.year)
  coldpool::make_raster_file(sph.predict %>%
                               akgfmaps::rasterize_and_mask(region_layers$survey.area),
                             filename = here::here("output", 
                                                   "raster", 
                                                   select.region, 
                                                   var.col, 
                                                   paste0(select.region, "_sph_", dat.year, "_", var.col, ".tif" )),
                             format = "GTiff",
                             overwrite = TRUE,
                             layer_name = dat.year)
  coldpool::make_raster_file(bes.predict %>%
                               akgfmaps::rasterize_and_mask(region_layers$survey.area),
                             filename = here::here("output", 
                                                   "raster", 
                                                   select.region, 
                                                   var.col, 
                                                   paste0(select.region, "_bes_", dat.year, "_", var.col, ".tif" )),
                             format = "GTiff",
                             overwrite = TRUE,
                             layer_name = dat.year)
  coldpool::make_raster_file(gau.predict %>%
                               akgfmaps::rasterize_and_mask(region_layers$survey.area),
                             filename = here::here("output", 
                                                   "raster", 
                                                   select.region, 
                                                   var.col, 
                                                   paste0(select.region, "_gau_", dat.year, "_", var.col, ".tif" )),
                             format = "GTiff",
                             overwrite = TRUE,
                             layer_name = dat.year)
  coldpool::make_raster_file(cir.predict %>%
                               akgfmaps::rasterize_and_mask(region_layers$survey.area),
                             filename =here::here("output", 
                                                  "raster", 
                                                  select.region, 
                                                  var.col, 
                                                  paste0(select.region, "_cir_", dat.year, "_", var.col, ".tif" )),
                             format = "GTiff",
                             overwrite = TRUE,
                             layer_name = dat.year)
  coldpool::make_raster_file(mat.predict %>%
                               akgfmaps::rasterize_and_mask(region_layers$survey.area),
                             filename = here::here("output", 
                                                   "raster", 
                                                   select.region, 
                                                   var.col, 
                                                   paste0(select.region, "_mat_", dat.year, "_", var.col, ".tif" )),
                             format = "GTiff",
                             overwrite = TRUE,
                             layer_name = dat.year)
  coldpool::make_raster_file(ste.predict %>% 
                               akgfmaps::rasterize_and_mask(region_layers$survey.area), 
                             filename = here::here("output", 
                                                   "raster", 
                                                   select.region, 
                                                   var.col, paste0(select.region, "_ste_", dat.year, "_", var.col, ".tif" )),
                             format = "GTiff", 
                             overwrite = TRUE, 
                             layer_name = dat.year)
  coldpool::make_raster_file(tps.predict %>%
                               akgfmaps::rasterize_and_mask(region_layers$survey.area),
                             filename = here::here("output", 
                                                   "raster", 
                                                   select.region, 
                                                   var.col, 
                                                   paste0(select.region, "_tps_", dat.year, "_", var.col, ".tif" )),
                             format = "GTiff",
                             overwrite = TRUE,
                             layer_name = dat.year)
}