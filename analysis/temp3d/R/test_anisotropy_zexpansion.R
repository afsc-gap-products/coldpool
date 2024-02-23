

z_expansion <- numeric()
rmse <- numeric()
kriging_model <- c("nn", "idw", "exp", "cir", "gau", "sph", "mat", "bes", "ste")

start_time <- Sys.time()
est_expansion <- estimate_z_expansion_new(z_start = 1e4, 
                                          x = x, 
                                          cv_index = x$fold, 
                                          location_formula = ~ LONGITUDE + LATITUDE + DEPTH,
                                          kriging_formula = TEMPERATURE ~ 1,
                                          vgm_width = 15000,
                                          nm = Inf,
                                          model = "Exp",
                                          use_for_rmse = x$CAST == "bottom")
end_time <- Sys.time()
print(end_time-start_time)

z_expansion <- c(z_expansion, est_expansion$par)
rmse <- c(rmse, est_expansion$value)


best_variogram_model <- dat$error_table$name[which.min(dat$error_table$RMSE)]

region <- "AI"

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
  
  anis <- coldpool::estimate_anisotropy(x = dplyr::filter(sel_profile, CAST == "bottom"),
                                        variable_name = "TEMPERATURE",
                                        latitude_name = "LATITUDE",
                                        longitude_name = "LONGITUDE",
                                        input_crs = "WGS84",
                                        interpolation_crs = crs_by_region$utmcrs[crs_by_region$region == region],
                                        nm = Inf,
                                        vgm_width = NULL,
                                        variogram_model = best_variogram_model,
                                        kriging_formula = variable_name ~ 1)
  
}