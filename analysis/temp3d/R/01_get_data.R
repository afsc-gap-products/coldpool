library(RODBC)
library(akgfmaps)
library(gapctd)
library(navmaps)


get_data_temp3d <- function() {
  
  channel <- gapctd::get_connected(schema = "AFSC")
  
  survey_definition_ids <- c(47, 52, 98, 143, 78)
  region_id <- c("GOA", "AI", "EBS", "NBS", "SLOPE")
  
  event_names <- data.frame(EVENT_TYPE_ID = c(3, 4, 6, 8, 9, 15, 16),
                            EVENT_NAME = c("on_bottom_time", "eq_time", "haulback_time", "doors_up_time",  "doors_out_time", "start_haul_time", "end_haul_time"))
  
  for(ii in 1:length(survey_definition_ids)) {
    
    dir.create(here::here("data", region_id[ii]))
    
    hauls <- RODBC::sqlQuery(
      channel = channel,
      query = paste0(
        "select rbh.cruise, rbh.vessel, rbh.haul, rbh.gear_temperature, rbh.bottom_depth, 
        rbh.surface_temperature, h.gear_depth, h.haul_id, rbh.start_time, rbh.start_latitude,
        rbh.start_longitude, rbh.end_latitude, rbh.end_longitude, rbh.performance, e.event_type_id, 
        e.date_time
        from race_data.hauls h, race_data.cruises c, race_data.surveys s, racebase.haul rbh, 
        race_data.events e
        where h.cruise_id = c.cruise_id 
        and s.survey_id = c.survey_id 
        and h.haul = rbh.haul 
        and c.vessel_id = rbh.vessel 
        and c.cruise = rbh.cruise 
        and rbh.performance >= 0 
        and rbh.abundance_haul = 'Y' 
        and e.haul_id = h.haul_id 
        and e.event_type_id in (3, 4, 6, 8, 9, 15, 16) 
        and s.survey_definition_id = ",
        survey_definition_ids[ii]
      )
    ) |>
      dplyr::mutate(DATE_TIME = lubridate::with_tz(lubridate::force_tz(DATE_TIME, tzone = "UTC"), tzone = "America/Anchorage"),
                    HAUL_ID = paste0(VESSEL, CRUISE, HAUL)) |>
      dplyr::inner_join(event_names, by = "EVENT_TYPE_ID") |>
      dplyr::select(-EVENT_TYPE_ID) |>
      tidyr::pivot_wider(values_from = "DATE_TIME", names_from = "EVENT_NAME")
    
    hauls$end_haul_time[is.na(hauls$end_haul_time)] <- hauls$doors_up_time[is.na(hauls$end_haul_time)] + 180
    hauls$start_haul_time[is.na(hauls$start_haul_time)] <- hauls$doors_up_time[is.na(hauls$start_haul_time)] - 180
    
    
    haul_midpoints <- navmaps::start_end_to_midpoint(start_latitude = hauls$START_LATITUDE,
                                   start_longitude = hauls$START_LONGITUDE,
                                   end_latitude = hauls$END_LATITUDE,
                                   end_longitude = hauls$END_LONGITUDE) |>
      as.data.frame()
    
    names(haul_midpoints) <- toupper(names(haul_midpoints))
    
    cast_locations <- hauls |>
      dplyr::bind_cols(haul_midpoints) |>
      tidyr::pivot_longer(cols = c(START_LATITUDE, START_LONGITUDE, END_LATITUDE, END_LONGITUDE, MID_LATITUDE, MID_LONGITUDE)) |>
      dplyr::inner_join(
        data.frame(name = c("START_LATITUDE", "START_LONGITUDE", "END_LATITUDE", "END_LONGITUDE", "MID_LATITUDE", "MID_LONGITUDE"),
                   CAST = c("downcast", "downcast", "upcast", "upcast", "bottom", "bottom"),
                   coord = c("LATITUDE", "LONGITUDE", "LATITUDE", "LONGITUDE", "LATITUDE", "LONGITUDE"))
      ) |>
      dplyr::select(HAUL_ID, START_TIME, BOTTOM_DEPTH, value, CAST, coord) |>
      tidyr::pivot_wider(names_from = c("coord"), values_from = value)
    
    temp_depth <- RODBC::sqlQuery(
      channel = channel,
      query = paste0(
        "select bt.temperature, bt.depth, bt.date_time, h.haul, c.vessel_id vessel, c.cruise
        from race_data.bathythermics bt, race_data.bathythermic_headers bth, race_data.hauls h, 
        race_data.cruises c, race_data.surveys s 
        where bth.haul_id = h.haul_id 
        and bt.bathythermic_header_id = bth.bathythermic_header_id 
        and h.cruise_id = c.cruise_id 
        and s.survey_id = c.survey_id 
        and bt.depth > 0.1 
        and bt.temperature > -2.0
        and bt.temperature < 20
        and s.survey_definition_id = ",
        survey_definition_ids[ii]
      )
    ) |> dplyr::mutate(DATE_TIME = lubridate::with_tz(lubridate::force_tz(DATE_TIME, tzone = "UTC"), tzone = "America/Anchorage"),
                       HAUL_ID = paste0(VESSEL, CRUISE, HAUL)) |>
      dplyr::select(-VESSEL, -CRUISE, -HAUL)
    
    temp_depth_rb2 <- RODBC::sqlQuery(channel = channel,
                                      query = paste0(
                                        "select bt.vessel, bt.cruise, bt.haul, bt.date_time,
                                        bt.temperature, bt.depth
                                        from race_edit.rb2_btd bt,
                                        racebase.haul h,
                                        race_data.cruises c,
                                        race_data.surveys s
                                        where bt.vessel = h.vessel
                                        and h.vessel = c.vessel_id
                                        and h.cruise = c.cruise
                                        and bt.cruise = h.cruise
                                        and bt.haul = h.haul
                                        and bt.depth > 0.1
                                        and bt.temperature > -2.0
                                        and bt.temperature < 20
                                        and s.survey_id = c.survey_id
                                        and s.survey_definition_id = ", survey_definition_ids[ii]))
    
    temp_depth_rb2 <- hauls |>
      dplyr::select(HAUL_ID) |> 
      dplyr::inner_join(temp_depth_rb2) |>
      dplyr::mutate(HAUL_ID = paste0(VESSEL, CRUISE, HAUL)) |>
      dplyr::select(-VESSEL, -CRUISE, -HAUL)
    
    temp_depth <- dplyr::filter(temp_depth, HAUL_ID %in% unique(hauls$HAUL_ID))
    
    hauls <- dplyr::filter(hauls, HAUL_ID %in% unique(temp_depth$HAUL_ID))
    
    temp_depth <- dplyr::bind_rows(temp_depth, temp_depth_rb2)
    
    unique_haul_id <- sort(unique(temp_depth$HAUL_ID))
    
    profile_dat <- data.frame()
    
    for(jj in 1:length(unique_haul_id)) {
      
      if(jj%%100 == 0) {
        message(paste0(jj, "/", length(unique_haul_id)))
      }

      sel_haul_temp <- dplyr::filter(temp_depth, HAUL_ID == unique_haul_id[jj])
      temp_depth <- dplyr::filter(temp_depth, HAUL_ID != unique_haul_id[jj])
      
      sel_haul_events <- dplyr::filter(hauls, HAUL_ID == unique_haul_id[jj])
      
      if(nrow(sel_haul_temp) < 10) {
        next
        }
      
      haul_temp_oce <- oce::as.oce(
        data.frame(datetime = sel_haul_temp$DATE_TIME,
                   temperature = sel_haul_temp$TEMPERATURE,
                   depth = sel_haul_temp$DEPTH) |>
          dplyr::arrange(datetime) |>
          dplyr::mutate(timeS = as.numeric(difftime(datetime, min(datetime), units = "secs")))
      ) |> 
        gapctd::lowpass_filter(variables = "temperature") # Low-pass filter temperature
      
      haul_temp_oce@metadata$startTime <- min(haul_temp_oce@data$datetime)
      
      if(survey_definition_ids[ii] %in% c(47, 52)) {
        
        dc_start <- sel_haul_events$start_haul_time
        dc_end <- sel_haul_events$eq_time
        uc_start <- sel_haul_events$haulback_time
        uc_end <- sel_haul_events$end_haul_time
        
      }
      
      if(survey_definition_ids[ii] %in% c(78, 98, 143)) {
        
        dc_start <- sel_haul_events$start_haul_time
        dc_end <- sel_haul_events$on_bottom_time
        uc_start <- sel_haul_events$haulback_time
        uc_end <- sel_haul_events$end_haul_time
        
      }
      
      dc_dat <- NULL
      uc_dat <- NULL
        
        dc_dat <- haul_temp_oce |>
          gapctd::section_oce(by = "datetime", start = dc_start, end = dc_end, cast_direction = "downcast") |>
          gapctd::bin_average(by = "depth", interpolate_missing = FALSE, exclude_bad_flag = FALSE)
        
        uc_dat <- haul_temp_oce |>
          gapctd::section_oce(by = "datetime", start = uc_start, end = uc_end, cast_direction = "upcast") |>
          gapctd::bin_average(by = "depth", interpolate_missing = FALSE, exclude_bad_flag = FALSE)
        

      
      bottom_dat <- haul_temp_oce |>
        gapctd::section_oce(by = "datetime", start = dc_end, end = uc_start, cast_direction = "bottom")
      
      if(!is.null(dc_dat)) {
        dc_dat <- as.data.frame(dc_dat@data) |>
          dplyr::select(depth, temperature)
        
        dc_dat$cast <- "downcast"
      }
      
      if(!is.null(uc_dat)) {
        uc_dat <- as.data.frame(uc_dat@data) |>
          dplyr::select(depth, temperature)
        
        uc_dat$cast <- "upcast"
      }
      
      if(!is.null(bottom_dat)) {
        bottom_dat <- data.frame(temperature = mean(bottom_dat@data$temperature, na.rm = TRUE),
                                 depth = mean(bottom_dat@data$depth, na.rm = TRUE))
        
        bottom_dat$cast <- "bottom"
      }
      
      if(any(c(!is.null(dc_dat), !is.null(uc_dat), !is.null(bottom_dat)))) {
        
        haul_dat <- dplyr::bind_rows(dc_dat, uc_dat, bottom_dat)
        
        names(haul_dat) <- toupper(names(haul_dat))
        
        haul_dat$HAUL_ID <- unique_haul_id[jj]
        
        profile_dat <- dplyr::bind_rows(profile_dat, haul_dat) 
        
      }
      

      
    }
    
    profile_dat <- dplyr::left_join(profile_dat, cast_locations)
    
    saveRDS(list(profile = profile_dat,
                 haul = hauls),
            file = here::here("data", 
                           region_id[ii], 
                           paste0("profile_data_", region_id[ii], ".rds")))
  }

  
}

get_data_temp3d()