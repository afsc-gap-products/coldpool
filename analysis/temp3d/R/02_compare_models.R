min_temp <- -2.2

# Setup
if(all(survey_definition_id == 47)) {
  utmcrs <- "EPSG:32605"
  region <- "GOA"
  min_year <- 1993
  subarea_levels <- c("Western Gulf of Alaska", "Eastern Gulf of Alaska") # Panel/timeseries order
}

if(all(survey_definition_id == 52)) {
  utmcrs <- "EPSG:32660"
  region <- "AI"
  min_year <- 1994
  subarea_levels <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians") # Panel/timeseries order
  min_temp <- 0 # Temporary solution to work around erroneous AI data
}

haul_data <- readRDS(here::here("data", region, paste0(region, "_akfin_haul.rds"))) |>
  dplyr::filter(GEAR_TEMPERATURE_C > min_temp)

# Fit models
fit_spmod <- function(data,
                      spcov_type,
                      model_formula,
                      anisotropy,
                      ...) {
  
  model_structure <- 
    expand.grid(
      spcov_type = spcov_type,
      formula = model_formula,
      anisotropy = anisotropy
    )
  
  model_structure$spcov_type <- as.character(model_structure$spcov_type)
  model_structure$convergence <- as.numeric(NA)
  model_structure$AICc <- as.numeric(NA)
  model_structure$bias <- as.numeric(NA)
  model_structure$mspe <- as.numeric(NA)
  model_structure$rmspe <- as.numeric(NA)
  model_structure$cor2 <- as.numeric(NA)
  model_structure$anis_rotate <- 0
  model_structure$anis_scale <- 1
  model_structure$npar <- as.numeric(NA)
  
  for(jj in 1:nrow(model_structure)) {
    
    mod_fit <- spmodel::splm(
      formula = model_structure$formula[[jj]],
      data = data,
      spcov_type = model_structure$spcov_type[jj],
      anisotropy = model_structure$anisotropy[jj],
      ...
    )
    
    model_structure$anis_rotate[jj] <- mod_fit$coefficients$spcov[['rotate']]
    model_structure$anis_scale[jj] <- mod_fit$coefficients$spcov[['scale']]
    
    model_structure$AICc[jj] <- spmodel::AICc(mod_fit)
    model_structure$convergence[jj] <- mod_fit$optim$convergence
    model_structure$npar[jj] <- mod_fit$npar
    model_structure[jj, c("bias", "mspe", "rmspe", "cor2")] <- spmodel::loocv(mod_fit, type = "response")
    
  }
  
  # Identify the most parsimonious model
  model_structure$id <- 1:nrow(model_structure)
  
  min_aicc <- min(model_structure$AICc, na.rm = TRUE)
  
  model_structure$delta_aicc <- model_structure$AICc - min_aicc
  
  candidate_models <- model_structure[model_structure$delta_aicc <= 2, ]
  
  parsimonious_model <- candidate_models[which.min(candidate_models$npar), ]
  
  model_structure$best <- model_structure$id %in% parsimonious_model$id
  
  model_structure$id <- NULL
  
  return(model_structure)
  
}

# Set latitude and longitude as the midpoint for each haul

haul_data$LATITUDE_DD_END[is.na(haul_data$LATITUDE_DD_END)] <- haul_data$LATITUDE_DD_START[is.na(haul_data$LATITUDE_DD_END)]
haul_data$LONGITUDE_DD_END[is.na(haul_data$LONGITUDE_DD_END)] <- haul_data$LONGITUDE_DD_START[is.na(haul_data$LONGITUDE_DD_END)]

towpath <- vector(mode = "list", length = length(haul_data))

for(ii in 1:nrow(haul_data)) {
  
  towpath[[ii]] <- c(
    sf::st_point(c(
      haul_data$LONGITUDE_DD_START[ii], 
      haul_data$LATITUDE_DD_START[ii])),
    sf::st_point(c(
      haul_data$LONGITUDE_DD_END[ii], 
      haul_data$LATITUDE_DD_END[ii]))
  ) |>
    sf::st_linestring()
  
}

towpath <- towpath |>
  sf::st_as_sfc(crs = "WGS84")

haul_data <- sf::st_sf(haul_data, geometry = towpath) |>
  navmaps::st_line_midpoints() |>
  sf::st_transform(crs = utmcrs)

# Fit candidate models

unique_years <- sort(unique(haul_data$YEAR))

model_performance_table <- data.frame()

for(jj in 1:length(unique_years)) {
  
  message(Sys.time(), " - Region: ", region, ", Year: ", unique_years[jj])
  
  # Fit gear temperature models in UTM
  model_performance_table <- fit_spmod(
    data = dplyr::filter(haul_data, YEAR == unique_years[jj]),
    spcov_type = c("exponential", "circular", "gaussian", "spherical", "matern"),
    model_formula = c(GEAR_TEMPERATURE_C ~ 1, 
                      GEAR_TEMPERATURE_C ~ DEPTH_M, 
                      GEAR_TEMPERATURE_C ~ DEPTH_M + I(DEPTH_M^2), 
                      GEAR_TEMPERATURE_C ~ I(log(DEPTH_M))),
    anisotropy = c(TRUE, FALSE), 
    estmethod = "ml"
  ) |>
    dplyr::mutate(YEAR = unique_years[jj], layer = "bottom") |>
    dplyr::bind_rows(model_performance_table)
  
  # Fit surface temperature models in UTM
  model_performance_table <- fit_spmod(
    data = dplyr::filter(haul_data, YEAR == unique_years[jj]),
    spcov_type = c("exponential", "circular", "gaussian", "spherical", "matern"),
    model_formula = c(SURFACE_TEMPERATURE_C ~ 1, 
                      SURFACE_TEMPERATURE_C ~ DEPTH_M, 
                      SURFACE_TEMPERATURE_C ~ DEPTH_M + I(DEPTH_M^2), 
                      SURFACE_TEMPERATURE_C ~ I(log(DEPTH_M))),
    anisotropy = c(TRUE, FALSE), 
    estmethod = "ml"
  ) |>
    dplyr::mutate(YEAR = unique_years[jj], layer = "surface") |>
    dplyr::bind_rows(model_performance_table)
  
}

model_performance_table$region <- region

saveRDS(object = model_performance_table,  
        file = here::here("output", region, paste0(region, "_splm_interp_performance.rds")))
