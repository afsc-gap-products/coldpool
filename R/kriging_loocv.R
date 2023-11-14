#' Compare interpolation methods using leave-one-out cross-validation
#' 
#' Conducts leave-one-out cross validation of a single predictor using kriging, nearest-neighbor, and inverse-distance weighting interpolation method using gstat.
#' 
#' @param x A data.frame containing variables to be interpolated and latitude and longitude coordinates.
#' @param variable_name 1L character vector indicating which varible to interpolate (e.g. 'TEMPERATURE')
#' @param latitude_name 1L character vector indicating the name of the latitude column (e.g., 'LATITUDE')
#' @param longitude_name 1L character vector indicating the name of the longitude column (e.g., 'LONGITUDE')
#' @param elevation_name Optional. 1L character vector indicating the name of the elevation/depth column. Only used for 3D kriging.
#' @param input_crs Character vector indicating the coordinate reference system for x (default = "WGS84")
#' @param interpolation_crs Coordinate reference system to use for interpolation
#' @param anisotopy_parameters Optional. Anisotropy parameters to use for ordinary kriging as a 2L vector. See: ?gstat::vgm. If NULL and estimate_anisotropy, anisotropy is estimated.
#' @param estimate_anisotropy Logical indicating whether anisotropy should be estimated for kriging methods based on k-fold cross validation. Default = FALSE
#' @param anisotropy_kfold 1L numeric vector indicating how many folds to use for cross-validation if estimate_anisotropy = TRUE.
#' @param nm Maximum number of nearest neighbor observations to use for interpolation.
#' @param vgm_width Optional. Width of variogram breaks.
#' @param kriging_formula Formula to use for kriging. Default (kriging_formula = variable_name ~ 1) is ordinary kriging without covariates. Formula for the gstat interface (see ?gstat::gstat). Covariate example: kriging_formula = variable_name ~ BOTTOM_DEPTH
#' @param interplation_methods Interpolation methods to use. Valid options are nearest-neighbor, inverse distance weighting, inverse distance weighting using the closest nm stations (idw_nmax), and kriging with and exponential (exp), circular (cir), gaussian (gau), Bessel (bes), Matern (mat), or Stein's Matern (ste) variogram model.
#' @param seed RNG seed (set.seed(seed)) to use for anisotropy estimation based on cross-validation.
#' @param only_return_anisotropy For internal use. Default = FALSE
#' @export 

kriging_loocv <- function(x,
                          variable_name,
                          latitude_name,
                          longitude_name,
                          elevation_name = NULL,
                          input_crs,
                          interpolation_crs,
                          anisotropy_parameters = NULL,
                          estimate_anisotropy = FALSE,
                          anisotropy_kfold = 10, 
                          nm = Inf,
                          vgm_width = NULL,
                          kriging_formula = variable_name ~ 1,
                          interpolation_methods = c("nn", "idw", "idw_nmax", "exp", "cir", "gau", "sph", "mat", "bes", "ste"),
                          seed = 19673,
                          only_return_anisotropy = FALSE
) {
  
  stopifnot("kriging_loocv: x must be a data.frame" = "data.frame" %in% class(x))
  stopifnot("kriging_loocv: variable_name column not found in x." = variable_name %in% names(x))
  stopifnot("kriging_loocv: latitude_name column not found in x." = latitude_name %in% names(x))
  stopifnot("kriging_loocv: longitude_name column not found in x." = longitude_name %in% names(x))
  
  interpolation_methods <- tolower(interpolation_methods)
  stopifnot("kriging_loocv: One or more interpolation_methods invalid." = all(interpolation_methods %in% c("nn", "idw", "idw_nmax", "exp", "cir", "gau", "sph", "mat", "bes", "ste")))
  
  interpolation_fits <- vector("list", 
                               length = length(interpolation_methods))
  names(interpolation_fits) <- interpolation_methods
  
  kriging_methods <- c("Exp", "Cir", "Gau", "Sph", "Mat", "Bes", "Ste")[match(interpolation_methods, c("exp", "cir", "gau", "sph", "mat", "bes", "ste"))]
  kriging_methods <- kriging_methods[!is.na(kriging_methods)]
  kriging_methods_lowercase <- tolower(kriging_methods)
  
  if(any(is.na(x$variable_name))) {
    message(paste0("kriging_loocv: Removing ", sum(is.na(x$variable_name)),
                   " variable_name NA values from data set"))
    x <- dplyr::filter(x, !is.na(variable_name))
  }
  
  input <- x
  
  names(x)[which(names(x) == variable_name)] <- "variable_name"
  names(x)[which(names(x) == latitude_name)] <- "latitude_name"
  names(x)[which(names(x) == longitude_name)] <- "longitude_name"
  location_formula <- ~ longitude_name + latitude_name
  
  if(!is.null(elevation_name)) {
    names(x)[which(names(x) == variable_name)] <- "elevation_name"
    location_formula <- ~ longitude_name + latitude_name + elevation_name
  }
  
  x <- sf::st_as_sf(x, 
                    coords = c("longitude_name", "latitude_name"),
                    crs = input_crs) |>
    sf::st_transform(crs = interpolation_crs)
  
  # Set variogram width equal to the 1% quantile for distances
  if(is.null(vgm_width)) {
    vgm_width <- as.numeric(quantile(sf::st_distance(x), 0.01))
  }
  
  x <- dplyr::bind_cols(x,
                        as.data.frame(sf::st_coordinates(x)) |>
                          dplyr::rename(longitude_name = X, latitude_name = Y)) |>
    sf::st_drop_geometry()
  
  mod_idw <- gstat::gstat(formula = kriging_formula, 
                          locations = location_formula,
                          data = x,
                          set = list(idp = 2), 
                          nmax = Inf)
  
  if("nn" %in% interpolation_methods) {
    mod <- gstat::gstat(formula = kriging_formula, 
                        locations = location_formula,
                        data = x,
                        set = list(idp = 0), 
                        nmax = nm)
    
    mod_fit <- gstat::gstat.cv(object = mod, 
                               verbose = FALSE, 
                               debug.level = 0, 
                               remove.all = TRUE)
    interpolation_fits[['nn']] <- mod_fit$var1.pred
    
  }
  
  if("idw" %in% interpolation_methods) {
    
    mod_fit <- gstat::gstat.cv(object = mod_idw, 
                               verbose = FALSE, 
                               debug.level = 0, 
                               remove.all = TRUE)
    interpolation_fits[['idw']] <- mod_fit$var1.pred
  }
  
  if("idw_nmax" %in% interpolation_methods) {
    mod <- gstat::gstat(formula = kriging_formula, 
                        locations = location_formula,
                        data = x, 
                        set = list(idp = 2), 
                        nmax = nm)
    
    mod_fit <- gstat::gstat.cv(object = mod, 
                               verbose = FALSE, 
                               debug.level = 0, 
                               remove.all = TRUE)
    interpolation_fits[['idw_nmax']] <- mod_fit$var1.pred
    
  }
  
  for(ii in 1:length(kriging_methods)) {
    
    if(estimate_anisotropy) {
      
      if(is.null(anisotropy_parameters)) {
        anisotropy_parameters <- c(90, 0.5)
      }
      
      x_mod <- mod_idw
      
      anisotropy_parameters <- try(
        optim(par = anisotropy_parameters,
              fn = coldpool:::fit_anisotropy_mse, 
              method = "L-BFGS-B",
              lower = c(0,0.01),
              upper = c(180,1),
              control = list(parscale = c(1, 0.1)),
              x_mod = mod_idw, 
              dat = x,
              vgm_width = vgm_width,
              kriging_formula = kriging_formula,
              location_formula = location_formula,
              nm = nm,
              kriging_method = kriging_methods[ii], 
              vgm_directions = c(0, 45, 90, 135), 
              kfold = anisotropy_kfold, 
              seed = seed)$par, 
        silent = TRUE)
      
      # Internal use, for estimating anisotropy
      if(only_return_anisotropy) {
        return(anisotropy_parameters)
      }
      
      if(class(anisotropy_parameters) == "try-error") {
        warning("kriging_loocv: Anisotropy estimation error. Assuming no anisotropy.")
        anisotropy_parameters <- NULL
      }
      
    }
    
    vgm_mod <- gstat::fit.variogram(gstat::variogram(mod_idw, 
                                                     width = vgm_width), 
                                    gstat::vgm(kriging_methods[ii]))
    
    if(!is.null(anisotropy_parameters)) {
      vgm_mod <- gstat::fit.variogram(gstat::variogram(mod_idw, 
                                                       width = vgm_width,
                                                       alpha = c(0, 45, 90, 135)), 
                                      gstat::vgm(model = kriging_methods[ii],
                                                 anis = anisotropy_parameters,
                                                 nugget = vgm_mod$psill[1]))
    } else {
      vgm_mod <- gstat::fit.variogram(gstat::variogram(mod_idw, 
                                                       width = vgm_width), 
                                      gstat::vgm(model = kriging_methods[ii],
                                                 nugget = vgm_mod$psill[1]))
    }

    
    mod <- gstat::gstat(formula = kriging_formula, 
                        locations = location_formula,
                        data = x,
                        model = vgm_mod, 
                        nmax = nm)
    
    mod_fit <- gstat::gstat.cv(object = mod, verbose = FALSE, debug.level = 0)
    interpolation_fits[[kriging_methods_lowercase[ii]]] <- mod_fit$var1.pred
    
  }
  
  return(cbind(input, 
               as.data.frame(interpolation_fits)))
  
}


#' Estimate anisotropy RMSE from k-fold cross validation
#' 
#' Internal function for estimating anisotropy based on cross-validation; wrapper around gstat functions.
#' 
#' @param anis_pars Anisotropy parameters as a 2L numeric vector indicating the angle and anisotropy ratio, e.g., c(90, 0.5)
#' @param x_mod A gstat object from which a variogram can be computed.
#' @param kriging_method Kriging method for gstat variogram.
#' @param kriging_formula Formula to use for kriging (formula argument in gstat::gstat).
#' @param location_formula Formula to use for location (location argument in gstat::gstat).
#' @param nm nmax for gstat
#' @param vgm_directions Directions for variogram (default = c(0, 45, 90, 135))
#' @param kfold Number of folds to use for cross-validation (default = 10)
#' @param vgm_width Width of variogram breaks.
#' @param seed Random number seed to use.
#' @noRd

fit_anisotropy_mse <- function(anis_pars,
                               x_mod, 
                               kriging_method, 
                               kriging_formula = ~ 1,
                               location_formula = ~ longitude_name + latitude_name,
                               nm,
                               vgm_width,
                               vgm_directions = c(0, 45, 90, 135), 
                               dat,
                               kfold = 10, 
                               seed = 19673) {
  
  vgm_mod <- gstat::fit.variogram(gstat::variogram(x_mod, 
                                                   width = vgm_width), 
                                  gstat::vgm(kriging_method))
  
  vgm_mod <- gstat::fit.variogram(gstat::variogram(x_mod, 
                                                   width = vgm_width,
                                                   alpha = vgm_directions), 
                                  gstat::vgm(model = kriging_method,
                                             anis = anis_pars,
                                             nugget = vgm_mod$psill[1]))
  
  
  mod <- gstat::gstat(formula = kriging_formula, 
                      locations = location_formula,
                      data = dat,
                      model = vgm_mod, 
                      nmax = nm)
  
  set.seed(seed)
  mse <- mean(gstat::gstat.cv(object = mod, 
                              verbose = FALSE, 
                              debug.level = 0, 
                              nfold = kfold)$residual^2)
  
  return(mse)
  
}