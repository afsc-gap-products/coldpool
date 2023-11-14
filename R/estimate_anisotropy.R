#' Estimate 2D variogram anisotropy parameters for kriging using 10-fold cross-validation
#' 
#' Uses the coldpool::kriging_loocv() interface.
#' 
#' @param x A data.frame containing variables to be interpolated and latitude and longitude coordinates.
#' @param variable_name 1L character vector indicating which varible to interpolate (e.g. 'TEMPERATURE')
#' @param latitude_name 1L character vector indicating the name of the latitude column (e.g., 'LATITUDE')
#' @param longitude_name 1L character vector indicating the name of the longitude column (e.g., 'LONGITUDE')
#' @param input_crs Character vector indicating the coordinate reference system for x (default = "WGS84")
#' @param interpolation_crs Coordinate reference system to use for interpolation.
#' @param kriging_formula Formula to use for kriging. Default (kriging_formula = variable_name ~ 1) is ordinary kriging without covariates. Formula for the gstat interface (see ?gstat::gstat). Covariate example: kriging_formula = variable_name ~ BOTTOM_DEPTH
#' @param starting_values Optional. Starting value of anisotropy parameters to use for optimization. See: ?gstat::vgm.
#' @param folds 1L numeric vector indicating how many folds to use for cross-validation.
#' @param nm Maximum number of nearest neighbor observations to use for interpolation.
#' @param vgm_width Optional. Width of variogram breaks.
#' @param variogram_model Character vector indicating which variogram model to use for interpolation. Valid options are exponential (exp), circular (cir), gaussian (gau), Bessel (bes), Matern (mat), or Stein's Matern (ste).
#' @param seed RNG seed (set.seed(seed)) to use for anisotropy estimation based on cross-validation.
#' @param only_return_anisotropy For internal use. Default = FALSE
#' @export 

estimate_anisotropy <- function(x,
                                variable_name,
                                latitude_name,
                                longitude_name,
                                input_crs,
                                interpolation_crs,
                                kriging_formula = variable_name ~ 1,
                                folds = 10,
                                variogram_model, 
                                starting_values = NULL,
                                vgm_width = NULL,
                                nm = Inf,
                                seed = 19673) {
  
  stopifnot("estimate_2d_anisotropy: Length of interpolation method must be 1." = length(variogram_model) == 1)
  stopifnot("estimate_2d_anisotropy: variogram_model must be one of 'Exp', 'Cir', 'Gau', 'Sph', 'Mat', 'Bes', 'Ste'" = tolower(variogram_model) %in% c('exp', 'cir', 'gau', 'sph', 'mat', 'bes', 'ste'))
  
  pars <- coldpool::kriging_loocv(x = x,
                                  variable_name = variable_name,
                                  latitude_name = latitude_name,
                                  longitude_name = longitude_name,
                                  elevation_name = NULL,
                                  input_crs = input_crs,
                                  interpolation_crs = interpolation_crs,
                                  anisotropy_parameters = starting_values,
                                  estimate_anisotropy = TRUE,
                                  nm = nm,
                                  vgm_width = vgm_width,
                                  kriging_formula = kriging_formula,
                                  interpolation_methods = variogram_model,
                                  seed = 19673,
                                  only_return_anisotropy = TRUE)
  
  return(pars)
  
}
