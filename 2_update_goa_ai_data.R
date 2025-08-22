# Update GOA and AI temperature products

library(coldpool)
library(akgfmaps)


# Function to calculate GOA/AI temperature products ------------------------------------------------
# Regional .rda files get written to /data/

# survey_definition_id is the RACE survey ID
# GOA = 47
# AI = 52

# max_year is the most recent survey

make_goa_ai_temp <- function(survey_definition_id, max_year, channel = NULL) {
  
  # Setup variables for analysis
  if(survey_definition_id == 47) {
    region <- "GOA"
    min_year <- 1993 # First year with temperature data from every haul
    range_baseline <- c(1993, 2014)
    subarea_levels <- c("Western Gulf of Alaska", "Eastern Gulf of Alaska") # Panel/timeseries order
  }
  
  if(survey_definition_id == 52) {
    region <- "AI"
    min_year <- 1991
    range_baseline <- c(1991, 2012)
    subarea_levels <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians") # Panel/timeseries order
  }
  
  channel <- coldpool:::get_connected(channel)
  
  # Get spatial data ---------------------------------------------------------------------------------
  # Survey layers
  map_layers <- 
    akgfmaps::get_base_layers(select.region = region, set.crs = "EPSG:3338")
  
  # ESR subareas
  esr_subareas <-
    akgfmaps::get_esr_regions(select.region = "esr_subarea", set.crs = "EPSG:3338") |>
    dplyr::filter(AREA_NAME %in% subarea_levels)
  
  # Retrieve cast data -------------------------------------------------------------------------------
  # Data from index station hauls with good performance that are used in stock assessments
  
  # Casts since 2005 are in RACE_DATA
  casts_since_2005 <- 
    RODBC::sqlQuery(
      channel = channel, 
      query =  
        paste0("select 
                c.vessel_id as vessel, 
                c.cruise, 
                h.haul, 
                bt.date_time,
                bt.depth,
                bt.temperature
              from 
                race_data.surveys s,
                race_data.cruises c,
                race_data.hauls h,
                race_data.bathythermic_headers bh, 
                race_data.bathythermics bt
              where s.survey_definition_id = ", survey_definition_id,
               "and s.survey_id = c.survey_id
                and c.cruise_id = h.cruise_id
                and h.abundance_haul = 'Y'
                and h.haul_id = bh.haul_id
                and bh.bathythermic_header_id = bt.bathythermic_header_id 
                and bt.datum_code in (0, 1, 7, 11)
                and bt.depth >= 0"
        )
    )
  
  # Cast data from before 2005 are in rb2 tables
  casts_before_2005 <- 
    RODBC::sqlQuery(
      channel = channel, 
      query =  
        paste0("select 
      c.vessel_id as vessel,
      c.cruise,
      h.haul,
      bt.date_time,
      bt.depth,
      bt.temperature
      from 
      gap_products.akfin_cruise c,
      gap_products.akfin_haul h,
      race_edit.rb2_btd bt
      where c.survey_definition_id = ", survey_definition_id,
               "and c.cruisejoin = h.cruisejoin
      and c.cruise = bt.cruise
      and h.haul = bt.haul
      and c.vessel_id = bt.vessel
      and bt.datum_code in (0, 1, 7, 11) 
      and bt.depth >= 0
      and bt.temperature between -2.2 and 30
      ")
    )
  
  # Remove data from casts that are in RACE_DATA
  casts_before_2005 <- 
    casts_before_2005 |>
    dplyr::anti_join(
      dplyr::select(
        casts_since_2005,
        VESSEL,
        CRUISE,
        HAUL
      ) |>
        unique()
    )
  
  casts <- 
    dplyr::bind_rows(
      casts_before_2005,
      casts_since_2005
    ) |>
    dplyr::arrange(DATE_TIME)
  
  # Retrieve event times -----------------------------------------------------------------------------
  events_since_2005 <- 
    RODBC::sqlQuery(
      channel = channel,
      query = 
        paste0("select 
              c.vessel_id as vessel, 
              c.cruise, 
              h.haul, 
              e.date_time,
              e.event_type_id
             from
              race_data.surveys s,
              race_data.cruises c,
              race_data.hauls h,
              race_data.events e
             where
              s.survey_definition_id = ", survey_definition_id,
               " and s.survey_id = c.survey_id
              and c.cruise_id = h.cruise_id
              and h.haul_id = e.haul_id
              and e.event_type_id in (3, 4, 6, 16)
             "
        )
    ) |>
    dplyr::mutate(
      EVENT_NAME = 
        dplyr::case_match(
          EVENT_TYPE_ID,
          3 ~ "OB",
          4 ~ "EQ",
          6 ~ "HB",
          16 ~ "END"
        )
    )
  
  events_before_2005 <-
    RODBC::sqlQuery(
      channel = channel,
      query = 
        paste0("select 
                c.vessel_id as vessel, 
                c.cruise, 
                h.haul, 
                sgt.date_time,
                sgt.time_flag,
                'EQ' as event_name
              from 
                gap_products.akfin_cruise c, 
                gap_products.akfin_haul h,
                race_edit.rb2_sgt sgt,
                race_edit.rb2_hpden hd
              where 
                c.survey_definition_id = 47
                and c.cruise = hd.cruise
                and c.vessel_id = hd.vessel
                and c.cruisejoin = h.cruisejoin 
                and h.haul = hd.haul
                and hd.vessel = sgt.vessel
                and hd.cruise = sgt.cruise
                and hd.haul = sgt.haul
                and hd.start_timelog = sgt.time_flag
                and sgt.datum_code = 1
              union
              select 
                c.vessel_id as vessel, 
                c.cruise, 
                h.haul, 
                sgt.date_time,
                sgt.time_flag event_type_id,
                'HB' as event_name
              from 
                gap_products.akfin_cruise c, 
                gap_products.akfin_haul h,
                race_edit.rb2_sgt sgt,
                race_edit.rb2_hpden hd
              where 
                c.survey_definition_id = 47
                and c.cruise = hd.cruise
                and c.vessel_id = hd.vessel
                and c.cruisejoin = h.cruisejoin 
                and h.haul = hd.haul
                and hd.vessel = sgt.vessel
                and hd.cruise = sgt.cruise
                and hd.haul = sgt.haul
                and hd.end_timelog = sgt.time_flag
                and sgt.datum_code = 1"
        )
      
    )
  
  events_before_2005 <- 
    events_before_2005 |>
    dplyr::anti_join(
      dplyr::select(
        events_since_2005,
        VESSEL,
        CRUISE,
        HAUL
      ) |>
        unique()
    )
  
  events <-
    dplyr::bind_rows(
      events_before_2005,
      events_since_2005
    )
  
  # Retrieve haul data from GAP_PRODUCTS -------------------------------------------------------------
  
  hauls <- 
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
               " and c.cruisejoin = h.cruisejoin"
        )
    )
  
  # Assign hauls to ESR subareas ---------------------------------------------------------------------
  # longitude cutoffs for the GOA
  
  haul_subareas <- 
    hauls |>
    dplyr::select(VESSEL, CRUISE, HAUL, LATITUDE_DD_START, LONGITUDE_DD_START) |>
    sf::st_as_sf(coords = c("LONGITUDE_DD_START", "LATITUDE_DD_START"), crs = "WGS84") |>
    sf::st_transform(crs = "EPSG:3338") |>
    sf::st_intersection(esr_subareas) |>
    dplyr::filter(!is.na(AREA_NAME)) # Exclude hauls outside of ESR subareas from temperature calculations
  
  haul_subareas <-
    haul_subareas |>
    dplyr::select(VESSEL, CRUISE, HAUL, SUBAREA = AREA_NAME) |>
    sf::st_drop_geometry()
  
  hauls <- 
    dplyr::inner_join(
      hauls, 
      haul_subareas, 
      by = c("VESSEL", "CRUISE", "HAUL")
    )
  
  hauls$SUBAREA <- factor(hauls$SUBAREA, levels = subarea_levels)
  
  
  # Calculate temperatures at target depths ----------------------------------------------------------
  
  # Depth interval around target depth for which measurements can be used to estimate temperature at the 
  # target depth
  start_time <- Sys.time()
  hauls$DC_TEMP_200M <- NA
  hauls$UC_TEMP_200M <- NA
  
  for(ii in 1:nrow(hauls)) {
    
    sel_cast <- 
      dplyr::filter(
        casts, 
        VESSEL == hauls$VESSEL[ii], 
        CRUISE == hauls$CRUISE[ii], 
        HAUL == hauls$HAUL[ii]
      )
    
    sel_events <- 
      dplyr::filter(
        events, 
        VESSEL == hauls$VESSEL[ii], 
        CRUISE == hauls$CRUISE[ii], 
        HAUL == hauls$HAUL[ii]
      )
    
    # Sort upcast and downcast by time. Algorithm requires sorting with shallower depths last
    upcast <- sel_cast[sel_cast$DATE_TIME >= (sel_events$DATE_TIME[sel_events$EVENT_NAME == "HB"] - 60), ] 
    upcast <- dplyr::arrange(upcast, DATE_TIME)
    downcast <- sel_cast[sel_cast$DATE_TIME <= (sel_events$DATE_TIME[sel_events$EVENT_NAME == "EQ"] + 60), ] 
    downcast <- dplyr::arrange(downcast, desc(DATE_TIME))
    
    hauls$UC_TEMP_200M[[ii]] <- 
      coldpool::calc_fixed_depth_var_bt(
        depth = upcast$DEPTH, 
        var = upcast$TEMPERATURE, 
        ref_depth = 200,
        ref_buffer = 10
      )
    
    hauls$DC_TEMP_200M[[ii]] <- 
      coldpool::calc_fixed_depth_var_bt(
        depth = downcast$DEPTH, 
        var = downcast$TEMPERATURE, 
        ref_depth = 200,
        ref_buffer = 10
      )
    
    if(ii %% 100 == 0) {
      print(paste0(ii, "/", nrow(hauls), ": ", round(as.numeric(difftime(Sys.time(), start_time, "min")), 2)))
      print(hauls[ii, ])
    }
    
  }
  
  # Assign temperature values ------------------------------------------------------------------------
  # Preference order: upcast > downcast > surface/gear temperature
  hauls <- hauls |>
    dplyr::mutate(
      TEMPERATURE_200M = 
        ifelse(
          !is.na(UC_TEMP_200M), 
          UC_TEMP_200M, 
          ifelse(
            !is.na(DC_TEMP_200M),
            DC_TEMP_200M, 
            ifelse(
              DEPTH_GEAR_M >= 195 & DEPTH_GEAR_M <= 205, 
              GEAR_TEMPERATURE_C,
              NA)
          )
        )
    )
  
  # Calculate temperature summaries ------------------------------------------------------------------
  # By for subarea, region, and year.
  
  temp_by_year <- 
    hauls |>
    dplyr::group_by(
      SUBAREA, 
      YEAR
    ) |>
    dplyr::summarise(
      MEAN_TEMPERATURE_200M = mean(TEMPERATURE_200M, na.rm = TRUE),
      MEAN_GEAR_TEMPERATURE = mean(GEAR_TEMPERATURE_C, na.rm = TRUE),
      MEAN_SURFACE_TEMPERATURE = mean(SURFACE_TEMPERATURE_C, na.rm = TRUE),
      SD_TEMPERATURE_200M = sd(TEMPERATURE_200M, na.rm = TRUE),
      SD_GEAR_TEMPERATURE = sd(GEAR_TEMPERATURE_C, na.rm = TRUE),
      SD_SURFACE_TEMPERATURE = sd(SURFACE_TEMPERATURE_C, na.rm = TRUE),
      N_200M = sum(!is.na(TEMPERATURE_200M)),
      N_GEAR = sum(!is.na(GEAR_TEMPERATURE_C)),
      N_SURFACE = sum(!is.na(SURFACE_TEMPERATURE_C)),
      SE_TEMPERATURE_200M = SD_TEMPERATURE_200M/sqrt(N_200M),
      SE_GEAR_TEMPERATURE = SD_GEAR_TEMPERATURE/sqrt(N_GEAR),
      SE_SURFACE_TEMPERATURE = SD_SURFACE_TEMPERATURE/sqrt(N_SURFACE),
    ) |>
    dplyr::mutate(
      MEAN_TEMPERATURE_200M = ifelse(is.na(SE_TEMPERATURE_200M), NA, MEAN_TEMPERATURE_200M),
      MEAN_GEAR_TEMPERATURE = ifelse(is.na(SE_GEAR_TEMPERATURE), NA, MEAN_GEAR_TEMPERATURE),
      MEAN_SURFACE_TEMPERATURE = ifelse(is.na(SE_SURFACE_TEMPERATURE), NA, MEAN_SURFACE_TEMPERATURE)
    ) |>
    dplyr::ungroup()
    
  # Temperature data for QA/QC evaluation
  write.csv(temp_by_year, here::here("assets", paste0(region, "_raw_temperature_table.csv"), row.names = FALSE))
  
  # Data product
  output_temperature <- 
    temp_by_year |>
    dplyr::filter(YEAR >= min_year) |>
    dplyr::select(
      YEAR,
      SUBAREA,
      MEAN_SURFACE_TEMPERATURE,
      MEAN_GEAR_TEMPERATURE,
      MEAN_200M_TEMPERATURE = MEAN_TEMPERATURE_200M,
      SE_SURFACE_TEMPERATURE = SE_SURFACE_TEMPERATURE,
      SE_GEAR_TEMPERATURE = SE_GEAR_TEMPERATURE,
      SE_200M_TEMPERATURE = SE_TEMPERATURE_200M
    ) |>
    dplyr::mutate(LAST_UPDATE = Sys.Date()) |>
    as.data.frame()
  
  # Save to rda
  
  if(survey_definition_id == 47) {
    goa_mean_temperature <- output_temperature
    usethis::use_data(goa_mean_temperature, overwrite = TRUE)
  }
  
  if(survey_definition_id == 52) {
    ai_mean_temperature <- output_temperature
    usethis::use_data(ai_mean_temperature, overwrite = TRUE)
  }
  
}

# Run function to get data -------------------------------------------------------------------------
channel <- coldpool::get_connected(schema = "AFSC")

make_goa_ai_temp(survey_definition_id = 47, max_year = 2025, channel = channel)
make_goa_ai_temp(survey_definition_id = 52, max_year = 2024, channel = channel)


# Build package
devtools::document(roclets = c('rd', 'collate', 'namespace'))
devtools::install(upgrade = "never", reload = TRUE)
devtools::build()
rstudioapi::restartSession()
library(coldpool)

