# library(coldpool)
# library(gapctd)
# library(navmaps)
# library(lubridate)

temperature_2d_cv <- function(region) {
  
  # UTM zones based on the most frequent zone among survey samples.
  crs_by_region <- data.frame(region = c("AI", "GOA", "EBS"),
                              utmcrs = c("EPSG:32660", "EPSG:32605", "EPSG:32602"),
                              aeacrs = "EPSG:3338")
  
  
  profile_dat <- readRDS(here::here("data", region, paste0("profile_data_", region, ".rds")))
  
  if(region == "EBS") {
    slope_dat <- readRDS(here::here("data", "slope", paste0("profile_data_slope.rds")))
    nbs_dat <- readRDS(here::here("data", "nbs", paste0("profile_data_nbs.rds")))
    
    profile_dat$profile <- dplyr::bind_rows(profile_dat$profile, slope_dat$profile, nbs_dat$profile)
    profile_dat$haul <- dplyr::bind_rows(profile_dat$haul, slope_dat$haul, nbs_dat$haul)
  }
  
  profile_dat$haul$YEAR <- lubridate::year(profile_dat$haul$START_TIME)
  
  unique_years <- sort(unique(profile_dat$haul$YEAR))
  
  loocv_fits <- data.frame()
  
  for(ii in 1:length(unique_years)) {
    
    sel_haul <- dplyr::filter(profile_dat$haul, 
                              YEAR == unique_years[ii], 
                              !is.na(GEAR_DEPTH))
    sel_profile <- dplyr::filter(profile_dat$profile, 
                                 HAUL_ID %in% unique(sel_haul$HAUL_ID),
                                 !is.na(LONGITUDE),
                                 !is.na(LATITUDE)) |>
      dplyr::inner_join(dplyr::select(sel_haul, HAUL_ID, BOTTOM_DEPTH, GEAR_DEPTH, YEAR)) |>
      dplyr::mutate(LOG_GEAR_DEPTH = log(GEAR_DEPTH))
    
    message("Kriging base")
    krige_base <- coldpool::kriging_loocv(
      x = dplyr::filter(sel_profile, CAST == "bottom"),
      variable_name = "TEMPERATURE",
      latitude_name = "LATITUDE",
      longitude_name = "LONGITUDE",
      elevation_name = NULL,
      input_crs = "WGS84",
      interpolation_crs = crs_by_region$utmcrs[crs_by_region$region == region],
      anisotropy_parameters = NULL,
      estimate_anisotropy = TRUE,
      nm = Inf,
      vgm_width = NULL,
      kriging_formula = variable_name ~ 1,
      interpolation_methods = c("nn", "idw", "exp", "cir", "gau", "sph", "mat", "bes", "ste")
    ) |>
      dplyr::mutate(MODEL = "No covariates")
    
    message("Kriging linear depth")
    krige_with_depth_linear <- coldpool::kriging_loocv(
      x = dplyr::filter(sel_profile, CAST == "bottom"),
      variable_name = "TEMPERATURE",
      latitude_name = "LATITUDE",
      longitude_name = "LONGITUDE",
      elevation_name = NULL,
      input_crs = "WGS84",
      interpolation_crs = crs_by_region$utmcrs[crs_by_region$region == region],
      anisotropy_parameters = NULL,
      estimate_anisotropy = TRUE,
      nm = Inf,
      vgm_width = NULL,
      kriging_formula = variable_name ~ GEAR_DEPTH,
      interpolation_methods = c("nn", "idw", "exp", "cir", "gau", "sph", "mat", "bes", "ste")
    ) |>
      dplyr::mutate(MODEL = "Linear depth")
    
    message("Kriging quadrtic")
    krige_with_depth_quadratic <- coldpool::kriging_loocv(
      x = dplyr::filter(sel_profile, CAST == "bottom"),
      variable_name = "TEMPERATURE",
      latitude_name = "LATITUDE",
      longitude_name = "LONGITUDE",
      elevation_name = NULL,
      input_crs = "WGS84",
      interpolation_crs = crs_by_region$utmcrs[crs_by_region$region == region],
      anisotropy_parameters = NULL,
      estimate_anisotropy = TRUE,
      nm = Inf,
      vgm_width = NULL,
      kriging_formula = variable_name ~ GEAR_DEPTH + I(GEAR_DEPTH^2),
      interpolation_methods = c("nn", "idw", "exp", "cir", "gau", "sph", "mat", "bes", "ste")
    ) |>
      dplyr::mutate(MODEL = "Quadratic depth")
    
    message("Kriging log depth")
    krige_with_log_depth <- coldpool::kriging_loocv(
      x = dplyr::filter(sel_profile, CAST == "bottom"),
      variable_name = "TEMPERATURE",
      latitude_name = "LATITUDE",
      longitude_name = "LONGITUDE",
      elevation_name = NULL,
      input_crs = "WGS84",
      interpolation_crs = crs_by_region$utmcrs[crs_by_region$region == region],
      anisotropy_parameters = NULL,
      estimate_anisotropy = TRUE,
      nm = Inf,
      vgm_width = NULL,
      kriging_formula = variable_name ~ LOG_GEAR_DEPTH,
      interpolation_methods = c("nn", "idw", "exp", "cir", "gau", "sph", "mat", "bes", "ste")
    ) |>
      dplyr::mutate(MODEL = "Log depth")
    
    dir.create(here::here("output", region), showWarnings = FALSE)
    
    saveRDS(object = list(krige_base = krige_base,
                          krige_with_depth_linear = krige_with_depth_linear,
                          krige_with_depth_quadratic = krige_with_depth_quadratic,
                          krige_with_log_depth = krige_with_log_depth),
            file = here::here("output", region, paste0("2d_interp_cv_", region, "_", unique_years[ii], ".rds"))
    )
    
    loocv_fits <- dplyr::bind_rows(loocv_fits,
                                   krige_base,
                                   krige_with_depth_linear,
                                   krige_with_depth_quadratic,
                                   krige_with_log_depth)
    
  }
  
}

temperature_2d_cv(region = "EBS")
temperature_2d_cv(region = "AI")
temperature_2d_cv(region = "GOA")
