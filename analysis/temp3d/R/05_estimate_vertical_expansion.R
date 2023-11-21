library(akgfmaps)
library(coldpool)
library(doParallel)
library(foreach)


est_vertical_expansion <- function(region, vgm_width) {
  
  profile_dat <- readRDS(list.files(here::here("data", region), full.names = TRUE, pattern = "cv_data")) |>
    dplyr::mutate(cv_index = CAST == "bottom")
  
  file_paths_aicc <- list.files(here::here("output", region), full.names = TRUE, pattern = "2d_interp_aicc")
  
  # UTM zones based on the most frequent zone among survey samples.
  crs_by_region <- data.frame(region = c("AI", "GOA", "EBS"),
                              utmcrs = c("EPSG:32660", "EPSG:32605", "EPSG:32602"),
                              aeacrs = "EPSG:3338")
  
  utm_crs <- crs_by_region$utmcrs[crs_by_region$region == region]
  
  
  # Setup four clusters and folds for each matchups
  doParallel::registerDoParallel(parallel::makeCluster(2))
  
  # folds <- caret::groupKFold(group = 1:length(file_paths_aicc))
  
  foreach::foreach(ii = 1:2) %dopar% { #length(file_paths_aicc)
    best_model <- readRDS(file_paths_aicc[ii])
    
    while(any(best_model$AICc[which.min(best_model$AICc)] - best_model$AICc[-which.min(best_model$AICc)] < -2)) {
      best_model <- best_model[-which.max(best_model$AICc), ]
    }
    
    while(!all(best_model$df == max(best_model$df))) {
      best_model <- best_model[-which.max(best_model$df), ]
    }
    
    best_model <- best_model[which.min(best_model$AICc), ]
    
    sel_profile <- dplyr::filter(profile_dat, YEAR == best_model$YEAR)
    
    z_expansion <- coldpool::estimate_z_expansion(x = sel_profile,
                                                  location_formula = ~ LONGITUDE + LATITUDE + DEPTH,
                                                  kriging_formula = best_model$formula[[1]],
                                                  model = paste0(toupper(substr(best_model$spcov_type, 1, 1)),
                                                                 tolower(substr(best_model$spcov_type, 2, 3))),
                                                  z_start = 5000,
                                                  cv_index = sel_profile$fold,
                                                  anisotropy_parameters = c(best_model$anis_rotate,0,0,best_model$anis_scale,1),
                                                    vgm_width = vgm_width,
                                                  nm = Inf,
                                                  maxdist = Inf,
                                                  use_for_mse = sel_profile$cv_index,
                                                  z_limits = c(10, 500000))

    best_model$z_expansion <- z_expansion$par
    
    saveRDS(object = best_model, file = here::here("output", region, paste0("3d_vert_expansion_est_", best_model$YEAR, ".rds")))

    return(best_model)
    
  }
}
  

est_vertical_expansion(region = "AI", vgm_width = 11000)
est_vertical_expansion(region = "GOA", vgm_width = 11000)
est_vertical_expansion(region = "EBS", vgm_width = 38000)


# x = sel_profile
# location_formula = ~ LONGITUDE + LATITUDE + DEPTH
# kriging_formula = best_model$formula[[1]]
# model = paste0(toupper(substr(best_model$spcov_type, 1, 1)),
#                tolower(substr(best_model$spcov_type, 2, 3)))
# z_start = 1000
# cv_index = sel_profile$fold
# anisotropy_parameters = c(best_model$anis_rotate,0,0,best_model$anis_scale,1)
# vgm_width = 11000
# nm = Inf
# maxdist = Inf
# use_for_mse = sel_profile$cv_index
# z_limits = c(10, 500000)
# 
# 
# estimate_z_expansion <- function(x,
#                                  location_formula,
#                                  kriging_formula,
#                                  model,
#                                  z_start,
#                                  cv_index, 
#                                  anisotropy_parameters = c(0,0,0,1,1), 
#                                  vgm_width,
#                                  nm = Inf,
#                                  maxdist = Inf,
#                                  use_for_mse = rep(TRUE, length(cv_index)),
#                                  z_limits = c(10, 500000)) {
#   
#   obj_fn( 
#   log_z_mult = log(1000),
#   dat = x,
#   cv_index = cv_index,
#   anisotropy_parameters = anisotropy_parameters,
#   location_formula = location_formula,
#   kriging_formula = kriging_formula,
#   vgm_width = vgm_width,
#   model = model,
#   use_for_mse = use_for_mse
#   )
#   
#   log_z_mult = log(1000)
#   dat = x
#   cv_index = cv_index
#   anisotropy_parameters = anisotropy_parameters
#   location_formula = location_formula
#   kriging_formula = kriging_formula
#   vgm_width = vgm_width
#   model = model
#   use_for_mse = use_for_mse
#   
#   
#   
#   obj_fn <- function(log_z_mult, 
#                      dat, 
#                      cv_index, 
#                      anisotropy_parameters, 
#                      location_formula,
#                      kriging_formula,
#                      vgm_width,
#                      model,
#                      use_for_mse) {
#     
#     dat[all.vars(location_formula)[3]] <- dat[all.vars(location_formula)[3]] * exp(log_z_mult)
#     
#     mod <- gstat::gstat(formula = kriging_formula, 
#                         locations = location_formula,
#                         data = dat,
#                         nmax = nm,
#                         maxdist = maxdist)
#     
#     vario_both <- gstat::variogram(mod, 
#                                    width = vgm_width,
#                                    alpha = c(0, 45, 90, 135))
#     
#     vario_nugget <- gstat::fit.variogram(gstat::variogram(mod, 
#                                                           width = vgm_width),
#                                          gstat::vgm(model))
#     
#     vario_fit <- gstat::fit.variogram(vario_both, 
#                                       gstat::vgm(model = model,
#                                                  anis = anisotropy_parameters,
#                                                  nugget = vario_nugget$psill[1]))
#     
#     mod <- gstat::gstat(formula = kriging_formula, 
#                         locations = location_formula,
#                         data = dat,
#                         nmax = nm,
#                         maxdist = maxdist,
#                         model = vario_fit)
#     
#     cv_results <- gstat::gstat.cv(object = mod, 
#                                   nfold = cv_index, 
#                                   verbose = FALSE, 
#                                   debug.level = 2)
#     
#     mse <- mean(cv_results$residual[use_for_mse]^2)
#     
#     return(mse)
#     
#   }
#   
#   results <- optim(par = log(z_start),
#                    fn = obj_fn,
#                    method = "L-BFGS-B",
#                    dat = x,
#                    control = list(trace = 5),
#                    kriging_formula = kriging_formula,
#                    location_formula = location_formula,
#                    anisotropy_parameters = anisotropy_parameters,
#                    lower = log(z_limits[1]),
#                    upper = log(z_limits[2]),
#                    cv_index = cv_index,
#                    vgm_width = vgm_width,
#                    model = model,
#                    use_for_mse = use_for_mse
#   )
#   
#   results$par <- exp(results$par)
#   
#   return(results)
#   
# }
# 
# 
# 
# 
