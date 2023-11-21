#' Estimate vertical expansion factor for 3D kriging
#' 
#' Uses L-BFGS algorithm implemented in optim().
#' 
#' @param z_start Starting value for vertical expansion estimation
#' @param x data.frame containing 
#' @param cv_index Index of folds for cross validation
#' @param location_formula Formula to use for location argument to gstat. See gstat documentation for description of the formula interface (see ?gstat::gstat).
#' @param kriging_formula Formula to use for kriging. See gstat documentation for description of the formula interface (see ?gstat::gstat).
#' @param anisotopy_parameters Anisotropy parameters to use for ordinary kriging as a 5L vector. See: ?gstat::vgm. If NULL and estimate_anisotropy, anisotropy is estimated.
#' @param vgm_width Optional.
#' @param variogram_model Character vector indicating which variogram model to use for interpolation. Valid options are exponential (exp), circular (cir), gaussian (gau), Bessel (bes), Matern (mat), or Stein's Matern (ste).
#' @param use_for_mse Logical vector indicating whether to use an observation to calculate MSE for cross-validation.
#' @param z_limits Upper and lower bounds for z_expansion, for optimization.
#' @param nm Maximum number of nearest neighbor observations to use for interpolation.
#' @export

estimate_z_expansion <- function(x,
                                 location_formula,
                                 kriging_formula,
                                 model,
                                 z_start,
                                 cv_index, 
                                 anisotropy_parameters = c(0,0,0,1,1), 
                                 vgm_width,
                                 nm = Inf,
                                 maxdist = Inf,
                                 use_for_mse = rep(TRUE, length(cv_index)),
                                 z_limits = c(10, 500000)) {
  
  
  
  obj_fn <- function(log_z_mult, 
                     dat, 
                     cv_index, 
                     anisotropy_parameters, 
                     location_formula,
                     kriging_formula,
                     vgm_width,
                     model,
                     use_for_mse) {
    
    dat[all.vars(location_formula)[3]] <- dat[all.vars(location_formula)[3]] * exp(log_z_mult)
    
    mod <- gstat::gstat(formula = kriging_formula, 
                        locations = location_formula,
                        data = dat,
                        nmax = nm,
                        maxdist = maxdist)
    
    vario_both <- gstat::variogram(mod, 
                                   width = vgm_width,
                                   alpha = c(0, 45, 90, 135))
    
    vario_nugget <- gstat::fit.variogram(gstat::variogram(mod, 
                                                          width = vgm_width),
                                         gstat::vgm(model))
    
    vario_fit <- gstat::fit.variogram(vario_both, 
                                      gstat::vgm(model = model,
                                                 anis = anisotropy_parameters,
                                                 nugget = vario_nugget$psill[1]))
    
    cv_results <- gstat::gstat.cv(object = mod, 
                                  nfold = cv_index, 
                                  verbose = FALSE, 
                                  debug.level = 2)
    
    mse <- mean(cv_results$residual[use_for_mse]^2)
    
    return(mse)
    
  }
  
  results <- optim(par = log(z_start),
                   fn = obj_fn,
                   method = "L-BFGS-B",
                   dat = x,
                   control = list(trace = 5),
                   kriging_formula = kriging_formula,
                   location_formula = location_formula,
                   anisotropy_parameters = anisotropy_parameters,
                   lower = log(z_limits[1]),
                   upper = log(z_limits[2]),
                   cv_index = cv_index,
                   vgm_width = vgm_width,
                   model = model,
                   use_for_mse = use_for_mse
  )
  
  results$par <- exp(results$par)
  
  return(results)
  
}