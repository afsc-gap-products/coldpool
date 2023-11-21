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
  doParallel::registerDoParallel(parallel::makeCluster(4))
  
  foreach::foreach(ii = 1:length(file_paths_aicc)) %dopar% {
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
