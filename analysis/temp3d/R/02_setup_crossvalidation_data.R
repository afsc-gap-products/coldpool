library(coldpool)
library(gapctd)
library(navmaps)
library(lubridate)


setup_crossvalidation_data <- function(region, depth_interval, min_depth, nfold, seed = 19673) {
  
  # UTM zones based on the most frequent zone among survey samples.
  crs_by_region <- data.frame(region = c("AI", "GOA", "EBS"),
                              utmcrs = c("EPSG:32660", "EPSG:32605", "EPSG:32602"),
                              aeacrs = "EPSG:3338")
  
  utm_crs <- crs_by_region$utmcrs[crs_by_region$region == region]
  
  profile_dat <- readRDS(here::here("data", region, paste0("profile_data_", region, ".rds")))
  
  if(region == "EBS") {
    slope_dat <- readRDS(here::here("data", "slope", paste0("profile_data_slope.rds")))
    nbs_dat <- readRDS(here::here("data", "nbs", paste0("profile_data_nbs.rds")))
    
    profile_dat$profile <- dplyr::bind_rows(profile_dat$profile, slope_dat$profile, nbs_dat$profile)
    profile_dat$haul <- dplyr::bind_rows(profile_dat$haul, slope_dat$haul, nbs_dat$haul)
  }
  
  profile_dat$haul$YEAR <- lubridate::year(profile_dat$haul$START_TIME)
  
  unique_years <- sort(unique(profile_dat$haul$YEAR))
  
  cross_validation_data <- data.frame()
  
  for(ii in 1:length(unique_years)) {
    
    sel_haul <- dplyr::filter(profile_dat$haul, 
                              YEAR == unique_years[ii], 
                              !is.na(GEAR_DEPTH))
    
    sel_profile <- dplyr::filter(profile_dat$profile, 
                                 HAUL_ID %in% unique(sel_haul$HAUL_ID),
                                 !is.na(LONGITUDE),
                                 !is.na(LATITUDE)) |>
      dplyr::inner_join(dplyr::select(sel_haul, HAUL_ID, BOTTOM_DEPTH, GEAR_DEPTH, YEAR),
                        by = c("HAUL_ID", "BOTTOM_DEPTH")) |>
      dplyr::mutate(index = as.numeric(factor(HAUL_ID)))
    
    sel_profile <- dplyr::filter(sel_profile, CAST %in% c("downcast", "upcast")) |>
      dplyr::inner_join(expand.grid(CAST = c("upcast", "downcast"),
                                    DEPTH = seq(min_depth, max(sel_profile$DEPTH), depth_interval)),
                        by = c("DEPTH", "CAST")) |>
      dplyr::bind_rows(dplyr::filter(sel_profile, CAST == "bottom")) |>
      dplyr::select(-START_TIME)
    
    sel_profile <- sf::st_as_sf(sel_profile, 
                                coords = c("LONGITUDE", "LATITUDE"),
                                crs = "WGS84") |>
      sf::st_transform(crs = utm_crs)
    
    sel_profile <- dplyr::bind_cols(sel_profile,
                                    as.data.frame(sf::st_coordinates(sel_profile)) |>
                                      dplyr::rename(LONGITUDE = X, 
                                                    LATITUDE = Y)) |>
      sf::st_drop_geometry()
    
    set.seed(seed)
    sel_profile$fold <- dplyr::inner_join(sel_profile,
                                          data.frame(HAUL_ID = unique(sel_profile$HAUL_ID),
                                                     fold = sample(rep(1:nfold, ceiling(length(unique(sel_profile$HAUL_ID))/nfold)), 
                                                                   size = length(unique(sel_profile$HAUL_ID)), 
                                                                   replace = FALSE)),
                                          by = "HAUL_ID")$fold
    
    cross_validation_data <- dplyr::bind_rows(cross_validation_data, sel_profile)
    
  }
  
  saveRDS(object = cross_validation_data, 
          file = here::here("data", region, paste0("cv_data_", region, ".rds")))
  
}

setup_crossvalidation_data(region  = "AI", 
                           depth_interval = 5, 
                           min_depth = 5, 
                           nfold = 10, 
                           seed = 19673)

setup_crossvalidation_data(region  = "EBS", 
                           depth_interval = 5, 
                           min_depth = 5, 
                           nfold = 10, 
                           seed = 19673)

setup_crossvalidation_data(region  = "GOA", 
                           depth_interval = 5, 
                           min_depth = 5, 
                           nfold = 10, 
                           seed = 19673)
