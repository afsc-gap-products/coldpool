library(coldpool)
library(gapctd)
library(navmaps)
library(lubridate)

temperature_2d_cv <- function(region) {
  
  kriging_methods <- c("exp", "cir", "gau", "sph", "mat", "bes", "ste")
  
  # UTM zones based on the most frequent zone among survey samples.
  crs_by_region <- data.frame(region = c("AI", "GOA", "EBS"),
                              utmcrs = c("EPSG:32660", "EPSG:32605", "EPSG:32602"),
                              aeacrs = "EPSG:3338")
  
  profile_dat <- readRDS(here::here("data", region, paste0("cv_data_", region, ".rds")))
  
  
  dir.create(here::here("output", region), showWarnings = FALSE)
  
  unique_years <- sort(unique(profile_dat$YEAR))
  
  cv_fits <- data.frame()
  
  for(ii in 1:length(unique_years)) {
    
    sel_profile <- dplyr::filter(profile_dat, 
                                 YEAR == unique_years[ii]) |>
      dplyr::filter(CAST == "bottom")
    
    message("Inverse distance weighting")
    nn_fit <- coldpool::kriging_cv(
      x = sel_profile,
      kriging_formula = TEMPERATURE ~ 1,
      location_formula = ~ LONGITUDE + LATITUDE,
      fold = sel_profile$fold,
      anisotropy_parameters = NULL,
      estimate_anisotropy = FALSE,
      nm = Inf,
      vgm_width = NULL,
      interpolation_methods = c("nn")
    ) |>
      dplyr::mutate(MODEL = "NN")
    
    message("Nearest-neighbor")
    idw_fit <- coldpool::kriging_cv(
      x = sel_profile,
      kriging_formula = TEMPERATURE ~ 1,
      location_formula = ~ LONGITUDE + LATITUDE,
      fold = sel_profile$fold,
      anisotropy_parameters = NULL,
      estimate_anisotropy = FALSE,
      nm = Inf,
      vgm_width = NULL,
      interpolation_methods = c("idw")
    ) |>
      dplyr::mutate(MODEL = "IDW")
    
    message("Kriging base")
    krige_base <- coldpool::kriging_cv(
      x = sel_profile,
      kriging_formula = TEMPERATURE ~ 1,
      location_formula = ~ LONGITUDE + LATITUDE,
      fold = sel_profile$fold,
      anisotropy_parameters = NULL,
      estimate_anisotropy = TRUE,
      nm = Inf,
      vgm_width = NULL,
      interpolation_methods = kriging_methods
    ) |>
      dplyr::mutate(MODEL = "OK")
    
    message("Kriging linear depth")
    krige_with_depth_linear <- coldpool::kriging_cv(
      x = sel_profile,
      kriging_formula = TEMPERATURE ~ GEAR_DEPTH,
      location_formula = ~ LONGITUDE + LATITUDE,
      fold = sel_profile$fold,
      anisotropy_parameters = NULL,
      estimate_anisotropy = TRUE,
      nm = Inf,
      vgm_width = NULL,
      interpolation_methods = kriging_methods
    ) |>
      dplyr::mutate(MODEL = "RK (Linear depth)")
    
    message("Kriging quadratic")
    krige_with_depth_quadratic <- coldpool::kriging_cv(
      x = sel_profile,
      kriging_formula = TEMPERATURE ~ GEAR_DEPTH + I(GEAR_DEPTH^2),
      location_formula = ~ LONGITUDE + LATITUDE,
      fold = sel_profile$fold,
      anisotropy_parameters = NULL,
      estimate_anisotropy = TRUE,
      nm = Inf,
      vgm_width = NULL,
      interpolation_methods = kriging_methods
    ) |>
      dplyr::mutate(MODEL = "RK (Quadratic depth)")
    
    message("Kriging log depth")
    krige_with_log_depth <- coldpool::kriging_cv(
      x = sel_profile,
      kriging_formula = TEMPERATURE ~ I(log(GEAR_DEPTH)),
      location_formula = ~ LONGITUDE + LATITUDE,
      fold = sel_profile$fold,
      anisotropy_parameters = NULL,
      estimate_anisotropy = TRUE,
      nm = Inf,
      vgm_width = NULL,
      interpolation_methods = kriging_methods
    ) |>
      dplyr::mutate(MODEL = "RK (Log depth)")
    
    
    saveRDS(object = dplyr::bind_rows(nn_fit,
                          idw_fit,
                          krige_base,
                          krige_with_depth_linear,
                          krige_with_depth_quadratic,
                          krige_with_log_depth), 
            file = here::here("output", region, paste0("2d_interp_cv_temperature_", region, "_", unique_years[ii], ".rds")))
    
    cv_fits <- dplyr::bind_rows(cv_fits,
                                nn_fit,
                                idw_fit,
                                krige_base,
                                krige_with_depth_linear,
                                krige_with_depth_quadratic,
                                krige_with_log_depth)
    
  }

  
  saveRDS(object = cv_fits, 
          file = here::here("output", region, paste0("2d_interp_cv_temperature_", region, ".rds"))
  )
  
  return(cv_fits)
  
}

ai_fits <- temperature_2d_cv(region = "AI")
ebs_fits <- temperature_2d_cv(region = "EBS")
goa_fits <- temperature_2d_cv(region = "GOA")
