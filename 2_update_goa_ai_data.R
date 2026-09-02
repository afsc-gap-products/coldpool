# Update AI and GOA temperature products

library(coldpool)

channel <- coldpool:::get_connected(schema = "AFSC")

# Set survey definition
survey_definition_id <- 47
min_temp <- 0.1


# Setup
if(all(survey_definition_id == 47)) {
  utmcrs <- "EPSG:32605"
  region <- "GOA"
  min_year <- 1993
  subarea_levels <- c("Western Gulf of Alaska", "Eastern Gulf of Alaska") # Panel/timeseries order
  
  bt_formula <- formula("GEAR_TEMPERATURE_C ~ I(log(DEPTH_M))")
  bt_spcov_type <- "circular"
  bt_anisotropy <- TRUE
  
  sst_formula <- formula("SURFACE_TEMPERATURE_C ~ DEPTH_M")
  sst_spcov_type <- "circular"
  sst_anisotropy <- TRUE
}

if(all(survey_definition_id == 52)) {
  utmcrs <- "EPSG:32660"
  region <- "AI"
  min_year <- 1994
  subarea_levels <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians") # Panel/timeseries order
  
  bt_formula <- formula("GEAR_TEMPERATURE_C ~ I(log(DEPTH_M))")
  bt_spcov_type <- "matern"
  bt_anisotropy <- TRUE
  
  sst_formula <- formula("SURFACE_TEMPERATURE_C ~ DEPTH_M + I(DEPTH_M^2)")
  sst_spcov_type <- "circular"
  sst_anisotropy <- TRUE
}

dir.create(here::here("output", "raster", region, "gear_temperature"), recursive = TRUE)
dir.create(here::here("output", "raster", region, "surface_temperature"), recursive = TRUE)

haul_data <- 
  RODBC::sqlQuery(
    channel = channel,
    query = 
      paste0("select 
      c.vessel_id as vessel, 
      c.cruise, 
      c.year,
      h.haul, 
      h.latitude_dd_start, 
      h.latitude_dd_end, 
      h.longitude_dd_start, 
      h.longitude_dd_end, 
      h.station, 
      h.stratum,
      h.depth_gear_m, 
      h.depth_m, 
      h.surface_temperature_c, 
      h.gear_temperature_c 
      from 
      gap_products.akfin_cruise c,
      gap_products.akfin_haul h 
             where survey_definition_id = ", survey_definition_id,
             " and c.cruisejoin = h.cruisejoin 
             and year >= ", min_year
      )
  )

saveRDS(haul_data, file = here::here("inst", "extdata", paste0(region, "_akfin_haul.rds")))

# Interpolate temperature data ---------------------------------------------------------------------

# Load spatial data that are needed to format and predict temperature within BTS areas

haul_data <- readRDS(here::here("inst", "extdata", paste0(region, "_akfin_haul.rds")))

map_layers <- 
  akgfmaps::get_base_layers(
    select.region = region, 
    set.crs = coldpool::ebs_proj_crs
  )

esr_subareas <-
  akgfmaps::get_esr_regions(select.region = "esr_subarea", set.crs = "EPSG:3338") |>
  dplyr::filter(AREA_NAME %in% subarea_levels)

bathy <- 
  system.file("extdata", "bathymetry.gpkg", package = "akgfmaps") |>
  terra::rast() |>
  terra::mask(map_layers$survey.area) |>
  terra::trim() 

# Create a UTM prediction grid
bathy_utm <- bathy |>
  terra::as.points(na.rm = TRUE) |>
  sf::st_as_sf() |>
  dplyr::rename(DEPTH_M = Height) |>
  sf::st_transform(crs = utmcrs)

# Predict temperatures in UTM CRS, convert back to AEA CRS -----------------------------------------
haul_data <- readRDS(here::here("inst", "extdata", paste0(region, "_akfin_haul.rds"))) |>
  dplyr::filter(GEAR_TEMPERATURE_C > min_temp)

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

start_time <- Sys.time()

unique_years <- sort(unique(haul_data$YEAR))

for(jj in 1:length(unique_years)) {
  
  sel_dat <- dplyr::filter(haul_data, YEAR == unique_years[jj])
  
  bt_mod <- 
    spmodel::splm(
      formula = bt_formula, 
      data = sel_dat, 
      spcov_type = bt_spcov_type,
      anisotropy = bt_anisotropy
    )
  
  sst_mod <- 
    spmodel::splm(
      formula = sst_formula, 
      data = sel_dat, 
      spcov_type = sst_spcov_type,
      anisotropy = sst_anisotropy
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
  
  if(region == "GOA" & unique_years[jj] == 2001) {
    
    # Eastern GOA wasn't sampled in 2001
    bt_rast <- 
      terra::mask(
        bt_rast,
        esr_subareas[esr_subareas$AREA_NAME == "Eastern Gulf of Alaska", ],
        inverse = TRUE
      )
    
  }
  
  if(jj == 1) {
    bt_layers <- bt_rast
    sst_layers <- sst_rast
  } else {
    bt_layers <- c(bt_layers, bt_rast)
    sst_layers <- c(sst_layers, sst_rast)
  }
  
  coldpool::make_raster_file(
    bt_rast,
    filename = here::here("output", "raster", region, "gear_temperature", paste0(region, "_", unique_years[jj], "_gear_temperature.tif")),
    format = "GTiff",
    overwrite = TRUE,
    layer_name = unique_years[jj]
  )
  
  coldpool::make_raster_file(
    sst_rast,
    filename = here::here("output", "raster", region, "surface_temperature", paste0(region, "_", unique_years[jj], "_surface_temperature.tif")),
    format = "GTiff",
    overwrite = TRUE,
    layer_name = unique_years[jj]
  )
  
}

# Region-specific data wrangling

if(region == "GOA") {
  
  goa_bottom_temperature <- terra::wrap(bt_layers)
  goa_surface_temperature <- terra::wrap(sst_layers)
  
  usethis::use_data(goa_bottom_temperature, overwrite = TRUE)
  usethis::use_data(goa_surface_temperature, overwrite = TRUE)
  
}

if(region == "AI") {
  
  ai_bottom_temperature <- terra::wrap(bt_layers)
  ai_surface_temperature <- terra::wrap(sst_layers)
  
  usethis::use_data(ai_bottom_temperature, overwrite = TRUE)
  usethis::use_data(ai_surface_temperature, overwrite = TRUE)
  
}
