library(akgfmaps)
library(coldpool)

region = "AI"

est_vertical_expansion <- function(region) {
  
  profile_dat <- readRDS(list.files(here::here("data", region), full.names = TRUE, pattern = "cv_data")) |>
    dplyr::mutate(cv_index = CAST == "bottom")
  
  file_paths_aicc <- list.files(here::here("output", region), full.names = TRUE, pattern = "2d_interp_aicc")
  
  # UTM zones based on the most frequent zone among survey samples.
  crs_by_region <- data.frame(region = c("AI", "GOA", "EBS"),
                              utmcrs = c("EPSG:32660", "EPSG:32605", "EPSG:32602"),
                              aeacrs = "EPSG:3338")
  
  utm_crs <- crs_by_region$utmcrs[crs_by_region$region == region]
  
  z_expansion <- data.frame()
  
  for(ii in 1:length(file_paths_aicc)) {
    
    print(ii)
    
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
                                                  z_start = 1e4,
                                                  cv_index = sel_profile$fold, 
                                                  anisotropy_parameters = c(best_model$anis_rotate,0,0,best_model$anis_scale,1), 
                                                  vgm_width = 11000,
                                                  nm = Inf,
                                                  maxdist = Inf,
                                                  use_for_mse = sel_profile$cv_index,
                                                  z_limits = c(10, 500000))
    
    print(z_expansion)
    best_model$z_expansion <- z_expansion
    
    z_expansion <- dplyr::bind_rows(z_expansion, best_model)
    
    saveRDS(object = z_expansion, file = here::here("output", region, "3d_vert_expansion_est_", best_model$YEAR, ".rds"))
    
  }
  
}







