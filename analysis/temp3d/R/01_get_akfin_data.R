library(coldpool)

channel <- coldpool:::get_connected(schema = "AFSC")

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

saveRDS(haul_data, file = here::here("data", region, paste0(region, "_akfin_haul.rds")))
