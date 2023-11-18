#' Compare interpolation methods using cross-validation
#' 
#' Conducts cross validation of a single predictor using kriging, nearest-neighbor, and inverse-distance weighting interpolation method using gstat.
#' 
#' @param x A data.frame containing variables to be interpolated and latitude and longitude coordinates.
#' @param fold Vector of numeric values indicating which observations are assigned to which fold. Default NULL performs leave-one-out cross validation. (see 'nfold' argument in ?gstat::gstat).
#' @param location_formula Formula to use for location argument to gstat. See gstat documentation for description of the formula interface (see ?gstat::gstat).
#' @param kriging_formula Formula to use for kriging. See gstat documentation for description of the formula interface (see ?gstat::gstat).
#' @param anisotopy_parameters Optional. Anisotropy parameters to use for ordinary kriging as a 2L vector for 2D kriging or 5L vector for 3D kriging. See: ?gstat::vgm. If NULL and estimate_anisotropy, anisotropy is estimated.
#' @param estimate_anisotropy Logical indicating whether anisotropy should be estimated for kriging methods based on k-fold cross validation. Default = FALSE
#' @param anisotropy_kfold 1L numeric vector indicating how many folds to use for cross-validation if estimate_anisotropy = TRUE.
#' @param nm Maximum number of nearest neighbor observations to use for interpolation.
#' @param vgm_width Optional. Width of variogram breaks.
#' @param interplation_methods Interpolation methods to use. Valid options are nearest-neighbor, inverse distance weighting, inverse distance weighting using the closest nm stations (idw_nmax), and kriging with and exponential (exp), circular (cir), gaussian (gau), Bessel (bes), Matern (mat), or Stein's Matern (ste) variogram model.
#' @param seed RNG seed (set.seed(seed)) to use for anisotropy estimation based on cross-validation.
#' @export 

kriging_cv <- function(x,
                       fold = NULL,
                       kriging_formula,
                       location_formula,
                       anisotropy_parameters = NULL,
                       nm = Inf,
                       maxdist = Inf,
                       interpolation_methods = c("nn", "idw", "exp", "cir", "gau", "sph", "mat", "bes", "ste"),
                       vgm_width = NULL,
                       estimate_anisotropy = FALSE,
                       only_return_anisotropy = FALSE,
                       anisotropy_kfold = 10, 

                       seed = 19673) {
  
  stopifnot("kriging_loocv: x must be a data.frame" = "data.frame" %in% class(x))
  
  interpolation_methods <- tolower(interpolation_methods)
  stopifnot("kriging_loocv: One or more interpolation_methods invalid." = all(interpolation_methods %in% c("nn", "idw", "exp", "cir", "gau", "sph", "mat", "bes", "ste")))
  
  interpolation_fits <- vector("list", 
                               length = length(interpolation_methods))
  names(interpolation_fits) <- interpolation_methods
  
  kriging_methods <- c("Exp", "Cir", "Gau", "Sph", "Mat", "Bes", "Ste")[match(interpolation_methods, c("exp", "cir", "gau", "sph", "mat", "bes", "ste"))]
  kriging_methods <- kriging_methods[!is.na(kriging_methods)]
  kriging_methods_lowercase <- tolower(kriging_methods)
  
  # Setup cross validation
  if(is.null(fold)) {
    fold <- 1:nrow(x)
  }
  
  
  if(is.null(vgm_width)) {
    warning("kriging_cv: vgm_width not provided. Calculating width of breaks for sample variogram as the 1% quantile of Euclidean distances among points.")
    vgm_width <- x |> 
      sf::st_as_sf(coords = all.vars(location_formula)) |>
      sf::st_distance() |>
      quantile(probs = 0.01) |>
      as.numeric()
  }
  
  mod_idw <- gstat::gstat(formula = kriging_formula, 
                          locations = location_formula,
                          data = x,
                          set = list(idp = 2), 
                          nmax = Inf,
                          maxdist = maxdist)
  
  if("nn" %in% interpolation_methods) {
    mod <- gstat::gstat(formula = kriging_formula, 
                        locations = location_formula,
                        data = x,
                        set = list(idp = 0), 
                        nmax = nm,
                        maxdist = maxdist)
    
    mod_fit <- gstat::gstat.cv(object = mod, 
                               nfold = fold,
                               verbose = FALSE, 
                               debug.level = 0, 
                               remove.all = TRUE)
    interpolation_fits[['nn']] <- mod_fit$var1.pred
    
  }
  
  if("idw" %in% interpolation_methods) {
    
    mod_fit <- gstat::gstat.cv(object = mod_idw, 
                               nfold = fold,
                               verbose = FALSE, 
                               debug.level = 0, 
                               remove.all = TRUE)
    interpolation_fits[['idw']] <- mod_fit$var1.pred
  }
  
  if(length(kriging_methods) < 1) {
    return(cbind(x, 
                 as.data.frame(interpolation_fits)))
  }
  
  for(ii in 1:length(kriging_methods)) {
    
    if(estimate_anisotropy) {
      
      if(is.null(anisotropy_parameters)) {
        
        message("kriging_cv: Conducting grid search to choose starting parameters for anisotropy estimation.")
        anis_ssq <- expand.grid(angle = seq(0,180,15),
                                ratio = seq(0.05, 1, 0.1),
                                ssq = as.numeric(NA))
        
        for(kk in 1:nrow(anis_ssq)) {
          anis_ssq$ssq[kk] <- coldpool:::fit_anisotropy_cv(anis_pars = c(anis_ssq$angle[kk], anis_ssq$ratio[kk]),
                                                           x_mod = mod_idw,
                                                           dat = x,
                                                           kriging_method = kriging_methods[ii], 
                                                           kriging_formula = kriging_formula,
                                                           location_formula = location_formula,
                                                           nm = nm,
                                                           vgm_width = vgm_width,
                                                           vgm_directions = c(0, 45, 90, 135),
                                                           seed = seed,
                                                           fold = fold)
        }
        
        anisotropy_parameters <- as.numeric(anis_ssq[which.min(anis_ssq$ssq), 1:2])
        
        message("kriging_cv: Anisotropy starting parameters: ", paste(anisotropy_parameters, collapse = ", "))
        
      }
      
      anis_fit <- optim(par = anisotropy_parameters,
                        fn = coldpool:::fit_anisotropy_cv, 
                        method = "L-BFGS-B",
                        lower = c(0, 1e-5),
                        upper = c(180, 1),
                        control = list(parscale = c(1,0.1)),
                        x_mod = mod_idw, 
                        dat = x,
                        vgm_width = vgm_width,
                        kriging_formula = kriging_formula,
                        location_formula = location_formula,
                        nm = nm,
                        kriging_method = kriging_methods[ii], 
                        vgm_directions = c(0, 45, 90, 135), 
                        fold = fold, 
                        seed = seed)
      
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
                        nmax = nm,
                        maxdist = maxdist)
    
    mod_fit <- gstat::gstat.cv(object = mod, 
                               nfold = fold, 
                               verbose = FALSE, 
                               debug.level = 0)
    interpolation_fits[[kriging_methods_lowercase[ii]]] <- mod_fit$var1.pred
    
  }
  
  return(cbind(x, 
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

fit_anisotropy_cv <- function(anis_pars,
                              x_mod, 
                              kriging_formula,
                              location_formula,
                              kriging_method, 
                              nm,
                              vgm_width,
                              vgm_directions = c(0, 45, 90, 135), 
                              dat,
                              fold = NULL,
                              seed = 19673) {
  
  if(is.null(fold)) {
    fold <- 1:nrow(dat)
  }
  
  vgm_mod <- gstat::fit.variogram(gstat::variogram(x_mod, 
                                                   width = vgm_width), 
                                  gstat::vgm(kriging_method))
  
  vgm_mod <- gstat::fit.variogram(gstat::variogram(x_mod, 
                                                   width = vgm_width,
                                                   alpha = vgm_directions), 
                                  gstat::vgm(model = kriging_method,
                                             anis = anis_pars,
                                             nugget = vgm_mod$psill[1])
  )
  
  mod <- gstat::gstat(formula = kriging_formula, 
                      locations = location_formula,
                      data = dat,
                      model = vgm_mod, 
                      nmax = nm)
  
  set.seed(seed)
  ssq <- sum(gstat::gstat.cv(object = mod, 
                             verbose = FALSE, 
                             debug.level = 0, 
                             nfold = fold)$residual^2)
  
  return(ssq)
  
}