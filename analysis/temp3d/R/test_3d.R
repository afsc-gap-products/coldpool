library(coldpool)
library(gapctd)
library(navmaps)
library(lubridate)

# UTM zones based on the most frequent zone among survey samples.
crs_by_region <- data.frame(region = c("AI", "GOA", "EBS"),
                            utmcrs = c("EPSG:32660", "EPSG:32605", "EPSG:32602"),
                            aeacrs = "EPSG:3338")

region = "AI"

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

ii <- 1

sel_haul <- dplyr::filter(profile_dat$haul, 
                          YEAR == unique_years[ii], 
                          !is.na(GEAR_DEPTH))
sel_profile <- dplyr::filter(profile_dat$profile, 
                             HAUL_ID %in% unique(sel_haul$HAUL_ID),
                             !is.na(LONGITUDE),
                             !is.na(LATITUDE)) |>
  dplyr::inner_join(dplyr::select(sel_haul, HAUL_ID, BOTTOM_DEPTH, GEAR_DEPTH, YEAR)) |>
  dplyr::mutate(LOG_GEAR_DEPTH = log(GEAR_DEPTH)) |>
  # dplyr::filter(CAST == "bottom") |>
  # dplyr::mutate(index = dplyr::row_number())
  dplyr::mutate(index = as.numeric(factor(HAUL_ID)))

max_depth <- dplyr::group_by(sel_profile, HAUL_ID) |>
  dplyr::filter(CAST != "bottom") |>
  dplyr::summarise(MAX_DEPTH=max(DEPTH))
sel_profile <- dplyr::inner_join(sel_profile, max_depth) |>
  dplyr::filter((MAX_DEPTH-DEPTH) < 7)

krige_base <- coldpool::kriging_loocv(
  x = sel_profile,
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
  interpolation_methods = c("nn", "idw", "exp")
) |>
  dplyr::mutate(MODEL = "Base")

krige_test_index <- coldpool::kriging_loocv(
  x = sel_profile,
  variable_name = "TEMPERATURE",
  latitude_name = "LATITUDE",
  longitude_name = "LONGITUDE",
  cv_index_name = "index",
  elevation_name = NULL,
  input_crs = "WGS84",
  interpolation_crs = crs_by_region$utmcrs[crs_by_region$region == region],
  anisotropy_parameters = NULL,
  estimate_anisotropy = TRUE,
  nm = Inf,
  vgm_width = NULL,
  kriging_formula = variable_name ~ 1,
  interpolation_methods = c("nn", "idw", "exp")
) |>
  dplyr::mutate(MODEL = "test_index")

krige_base$exp == krige_test_index$exp
krige_base$idw == krige_test_index$idw
krige_base$nn == krige_test_index$nn

krige_test_vert <- coldpool::kriging_loocv(
  x = dplyr::mutate(sel_profile, DEPTH = DEPTH * 1000),
  variable_name = "TEMPERATURE",
  latitude_name = "LATITUDE",
  longitude_name = "LONGITUDE",
  cv_index_name = "index",
  elevation_name = "DEPTH",
  input_crs = "WGS84",
  interpolation_crs = crs_by_region$utmcrs[crs_by_region$region == region],
  anisotropy_parameters = NULL,
  estimate_anisotropy = FALSE,
  nm = Inf,
  vgm_width = NULL,
  kriging_formula = variable_name ~ 1,
  interpolation_methods = c("nn", "idw", "exp")
) |>
  dplyr::mutate(MODEL = "test_vert")


krige_test_vert_no_index <- coldpool::kriging_loocv(
  x = dplyr::mutate(sel_profile, DEPTH = DEPTH * 1000),
  variable_name = "TEMPERATURE",
  latitude_name = "LATITUDE",
  longitude_name = "LONGITUDE",
  cv_index_name = NULL,
  elevation_name = "DEPTH",
  input_crs = "WGS84",
  interpolation_crs = crs_by_region$utmcrs[crs_by_region$region == region],
  anisotropy_parameters = NULL,
  estimate_anisotropy = FALSE,
  nm = Inf,
  vgm_width = NULL,
  kriging_formula = variable_name ~ 1,
  interpolation_methods = c("nn", "idw", "exp")
) |>
  dplyr::mutate(MODEL = "test_vert_no_index")

start_time <- Sys.time()
krige_test_vert_anisotropy <- coldpool::kriging_loocv(
  x = dplyr::mutate(sel_profile, DEPTH = DEPTH * 1000),
  variable_name = "TEMPERATURE",
  latitude_name = "LATITUDE",
  longitude_name = "LONGITUDE",
  cv_index_name = NULL,
  elevation_name = "DEPTH",
  input_crs = "WGS84",
  interpolation_crs = crs_by_region$utmcrs[crs_by_region$region == region],
  anisotropy_parameters = c(63.433424, 0, 0, 0.565689, 1), #anis = c(p,s) is equivalent to anis = c(p,0,0,s,1)
  estimate_anisotropy = FALSE,
  nm = Inf,
  vgm_width = 20000,
  kriging_formula = variable_name ~ 1,
  interpolation_methods = c("exp")
) |>
  dplyr::mutate(MODEL = "test_vert_anisotropy")
end_time <- Sys.time()
end_time-start_time
