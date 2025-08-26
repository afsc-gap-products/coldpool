library(coldpool)
library(spmodel)

survey_definition_id <- 47

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
  min_year <- 1991
  subarea_levels <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians") # Panel/timeseries order
}

if(all(survey_definition_id == 98)) {
  utmcrs <- "EPSG:32602"
  region <- "SEBS"
  min_year <- 1982
}

if(all(survey_definition_id == 143)) {
  utmcrs <- "EPSG:32602"
  region <- "NBS"
  min_year <- 2010
}

if(all(survey_definition_id %in% c(143, 98))) {
  utmcrs <- "EPSG:32602"
  region <- "EBS"
  min_year <- 1982
}

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
  
aicc_table <- data.frame()

haul_data <- readRDS(here::here("data", region, paste0(region, "_akfin_haul.rds"))) |>
  sf::st_as_sf(coords = c("LONGITUDE_DD_START", "LATITUDE_DD_START"), crs = "WGS84") |>
  sf::st_transform(crs = utmcrs)

unique_years <- sort(unique(haul_data$YEAR))

for(jj in 1:length(unique_years)) {
  
  message("Region: ", region, ", Year: ", unique_years[jj])
  
  # Fit gear temperature models in UTM
  aicc_table <- fit_spmod(
    data = dplyr::filter(haul_data, YEAR == unique_years[jj]),
    spcov_type = c("exponential", "circular", "gaussian", "spherical", "matern"),
    model_formula = c(I(log(GEAR_TEMPERATURE_C)) ~ 1, 
                      I(log(GEAR_TEMPERATURE_C)) ~ DEPTH_M, 
                      I(log(GEAR_TEMPERATURE_C)) ~ DEPTH_M + I(DEPTH_M^2), 
                      I(log(GEAR_TEMPERATURE_C)) ~ I(log(DEPTH_M))),
    anisotropy = c(TRUE, FALSE), 
    estmethod = "ml"
  ) |>
    dplyr::mutate(YEAR = unique_years[jj], layer = "bottom")
  dplyr::bind_rows(aicc_table)
  
  # Fit surface temperature models in UTM
  aicc_table <- fit_spmod(
    data = dplyr::filter(haul_data, YEAR == unique_years[jj]),
    spcov_type = c("exponential", "circular", "gaussian", "spherical", "matern"),
    model_formula = c(I(log(SURFACE_TEMPERATURE_C)) ~ 1, 
                      I(log(SURFACE_TEMPERATURE_C)) ~ DEPTH_M, 
                      I(log(SURFACE_TEMPERATURE_C)) ~ DEPTH_M + I(DEPTH_M^2), 
                      I(log(SURFACE_TEMPERATURE_C)) ~ I(log(DEPTH_M))),
    anisotropy = c(TRUE, FALSE), 
    estmethod = "ml"
  ) |>
    dplyr::mutate(YEAR = unique_years[jj], layer = "surface")
  dplyr::bind_rows(aicc_table)
  
}

saveRDS(object = aicc_table,  
        file = here::here("output", region, paste0(region, "_splm_interp_aicc.rds")))

# Function to select the best fit model for interpolation
map_layers <- 
  akgfmaps::get_base_layers(
    select.region = "goa", 
    set.crs = coldpool::ebs_proj_crs
  )

# Load ESR subareas 
esr_subareas <-
  akgfmaps::get_esr_regions(select.region = "esr_subarea", set.crs = "EPSG:3338") |>
  dplyr::filter(AREA_NAME %in% subarea_levels)

# Load bathymetry raster, mask to survey extent, trim whitespace, convert to sf, change depth column name to match model
bathy <- 
  system.file("extdata", "bathymetry.gpkg", package = "akgfmaps") |>
  terra::rast() |>
  terra::mask(map_layers$survey.area) |>
  terra::trim() 

# Create a UTM 
bathy_utm <- bathy |>
  terra::as.points(na.rm = TRUE) |>
  sf::st_as_sf() |>
  dplyr::rename(DEPTH_M = Height) |>
  sf::st_transform(crs = utmcrs)

# Predict temperatures in UTM CRS, convert back to AEA CRS
start_time <- Sys.time()

for(jj in 1:length(unique_years)) {
  
  sel_dat <- dplyr::filter(haul_data, YEAR == unique_years[jj])
  
  best_bt_model <- 
    dplyr::filter(
      aicc_table, 
      YEAR == unique_years[jj],
      layer == "bottom",
      best == TRUE
      )
  
  bt_mod <- 
    spmodel::splm(
      formula = best_bt_model$formula[1], 
      data = sel_dat, 
      spcov_type = best_bt_model$spcov_type[1],
      anisotropy = best_bt_model$anisotropy[1]
    )
  
  best_sst_model <- 
    dplyr::filter(
      aicc_table, 
      YEAR == unique_years[jj],
      layer == "surface",
      best == TRUE
    )
  
  sst_mod <- 
    spmodel::splm(
      formula = best_sst_model$formula[1], 
      data = sel_dat, 
      spcov_type = best_sst_model$spcov_type[1],
      anisotropy = best_sst_model$anisotropy[1]
    )
  
  bathy_utm[["BT"]] <- predict(bt_mod, newdata = bathy_utm)
  bathy_utm[["SST"]] <- predict(sst_mod, newdata = bathy_utm)
  
  end_time <- Sys.time()
  print(difftime(end_time, start_time))
  
  bt_rast <- 
    dplyr::select(bathy_utm, BT) |>
    sf::st_transform(crs = "EPSG:3338") |>
    terra::rasterize(y = bathy, field = "BT")
  
  varnames(bt_rast) <- "gear_temperature"
  
  names(bt_rast) <- unique_years[jj]
  
  sst_rast <- 
    dplyr::select(bathy_utm, SST) |>
    sf::st_transform(crs = "EPSG:3338") |>
    terra::rasterize(y = bathy, field = "SST")
  
  varnames(sst_rast) <- "surface_temperature"
  
  names(sst_rast) <- unique_years[jj]
  
  if(jj == 1) {
    bt_layers <- bt_rast
    sst_layers <- sst_rast
  } else {
    bt_layers <- c(bt_layers, bt_rast)
    sst_layers <- c(sst_layers, sst_rast)
  }
  
}


# Regional clean up

if(region == "goa") {
  
  # Eastern GOA wasn't sampled in 2001
  bt_layers["2001"] <- 
    mask(
      bt_layers["2001"], , 
      esr_subareas[esr_subareas$AREA_NAME == "Eastern Gulf of Alaska", ],
      inverse = TRUE
    )
  
}


saveRDS(bt_layers, here::here("output", paste0(region, "_bt.rds")))
saveRDS(sst_layers, here::here("output", paste0(region, "_sst.rds")))