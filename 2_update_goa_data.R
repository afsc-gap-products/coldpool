# Update GOA and AI temperature products

library(coldpool)
library(akgfmaps)

# Retrieve GOA or AI data
survey_definition_id <- 47 #GOA
# survey_definition_id <- 52 #AI

max_year <- 2025

channel <- coldpool:::get_connected(schema = "AFSC")

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

# Casts data from before 2005 are in racebase 2
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
  dplyr::mutate(EVENT_NAME = 
                  dplyr::case_match(
                    EVENT_TYPE_ID,
                    3 ~ "OB",
                    4 ~ "EQ",
                    6 ~ "HB",
                    16 ~ "END"
                  ))


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

rm(events_since_2005, events_before_2005)

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
      h.depth_gear_m, 
      h.depth_m, 
      h.surface_temperature_c, 
      h.gear_temperature_c 
      from 
      gap_products.akfin_cruise c,
      gap_products.akfin_haul h 
             where survey_definition_id = ", survey_definition_id,
             "and c.cruisejoin = h.cruisejoin"
      )
  )

# Assign hauls to subareas
if(survey_definition_id == 47) {
  hauls$SUBAREA <- "EGOA"
  hauls$SUBAREA[hauls$LONGITUDE_DD_START < -147] <- "CGOA"
  hauls$SUBAREA[hauls$LONGITUDE_DD_START < -165] <- "WGOA"
}


# Calculate temperatures at target depths from upcasts and downcasts

start_time <- Sys.time()
hauls$DC_TEMP_5M <- -99
hauls$DC_TEMP_100M <- -99
hauls$DC_TEMP_200M <- -99
hauls$UC_TEMP_5M <- -99
hauls$UC_TEMP_100M <- -99
hauls$UC_TEMP_200M <- -99

temp_cast <- casts

for(ii in 1:nrow(hauls)) {
  
  if(ii %% 200 == 0) {
    print(paste0(ii, "/", nrow(hauls), ": ", round(as.numeric(difftime(Sys.time(), start_time, "min")), 2)))
  }
  
  
  sel_cast <- 
    dplyr::filter(
      temp_cast, 
      VESSEL == hauls$VESSEL[ii], 
      CRUISE == hauls$CRUISE[ii], 
      HAUL == hauls$HAUL[ii]
    )
  
  temp_cast <- 
    dplyr::filter(
      temp_cast, 
      VESSEL != hauls$VESSEL[ii], 
      CRUISE != hauls$CRUISE[ii], 
      HAUL != hauls$HAUL[ii]
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
  
  if(nrow(upcast) > 3) {
    
    hauls$UC_TEMP_5M[[ii]] <- 
      coldpool::calc_fixed_depth_var_bt(
        depth = upcast$DEPTH, 
        var = upcast$TEMPERATURE, 
        ref_depth = 5
      )
    
    hauls$UC_TEMP_100M[[ii]] <- 
      coldpool::calc_fixed_depth_var_bt(
        depth = upcast$DEPTH, 
        var = upcast$TEMPERATURE, 
        ref_depth = 100
      )
    
    hauls$UC_TEMP_200M[ii] <- 
      coldpool::calc_fixed_depth_var_bt(
        depth = upcast$DEPTH, 
        var = upcast$TEMPERATURE, 
        ref_depth = 200
      )
    
  }
  
  if(nrow(downcast) > 3) {
    
    hauls$DC_TEMP_5M[[ii]] <- 
      coldpool::calc_fixed_depth_var_bt(
        depth = downcast$DEPTH, 
        var = downcast$TEMPERATURE, 
        ref_depth = 5
      )
    
    hauls$DC_TEMP_100M[ii] <- 
      coldpool::calc_fixed_depth_var_bt(
        depth = downcast$DEPTH, 
        var = downcast$TEMPERATURE, 
        ref_depth = 100
      )
    
    hauls$DC_TEMP_200M[ii] <- 
      coldpool::calc_fixed_depth_var_bt(
        depth = downcast$DEPTH, 
        var = downcast$TEMPERATURE, 
        ref_depth = 200
      )
    
  }
}

# Replace dummy values (-99) with NAs
hauls$DC_TEMP_5M[is.na(hauls$DC_TEMP_5M)] <- NA
hauls$DC_TEMP_100M[is.na(hauls$DC_TEMP_100M)] <- NA
hauls$DC_TEMP_200M[is.na(hauls$DC_TEMP_200M)] <- NA
hauls$UC_TEMP_5M[is.na(hauls$UC_TEMP_5M)] <- NA
hauls$UC_TEMP_100M[is.na(hauls$UC_TEMP_100M)] <- NA
hauls$UC_TEMP_200M[is.na(hauls$UC_TEMP_200M)] <- NA