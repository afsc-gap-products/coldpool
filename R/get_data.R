#' Get gear temperature data from racebase and write to /data/
#' 
#' @param channel ODBC connection as an RODBC class
#' @param include_preliminary_data Character vector indicating whether to include preliminary data from the EBS ("ebs") and NBS ("nbs") Should the query return preliminary data from the current year from the race_data.edit_* tables? If this is used, the sql script needs to be updated for the current year.
#' @export

get_data <- function(channel, include_preliminary_data = NULL) {
  
  temperature_all_hauls_df <- RODBC::sqlQuery(channel, coldpool::sql_to_rqry(system.file("sql", 
                                                                                         "ebs_gear_temperature_all_hauls.sql", 
                                                                                         package = "coldpool"))) %>%
    dplyr::mutate(LATITUDE = (START_LATITUDE + END_LATITUDE)/2,
                  LONGITUDE = (START_LONGITUDE + END_LONGITUDE)/2,
                  YEAR = floor(CRUISE/100)) %>%
    dplyr::select(-START_LATITUDE, 
                  -END_LATITUDE, 
                  -START_LONGITUDE, 
                  -END_LONGITUDE) %>%
    dplyr::filter(!is.na(GEAR_TEMPERATURE), 
                  !is.na(LATITUDE), 
                  !is.na(LONGITUDE))
  
  names(temperature_all_hauls_df) <- tolower(names(temperature_all_hauls_df))
  
  if(any(tolower(include_preliminary_data == "nbs"))) {
    prelim_temperature_all_hauls_df <- RODBC::sqlQuery(channel, 
                                                       coldpool::sql_to_rqry(system.file("sql", "ebs_gear_temperature_all_hauls_current_year.sql", package = "coldpool"))) %>%
      dplyr::mutate(LATITUDE = coldpool::convert_ddm_to_dd(LATITUDE_DMS),
                    LONGITUDE = coldpool::convert_ddm_to_dd(LONGITUDE_DMS),
                    YEAR = floor(CRUISE/100)) %>%
      dplyr::select(-LATITUDE_DMS, -LONGITUDE_DMS, -CRUISE_ID, -EVENT_TYPE_ID, -SURVEY_DEFINITION_ID) %>%
      dplyr::filter(!is.na(GEAR_TEMPERATURE), !is.na(LATITUDE), !is.na(LONGITUDE))
    
    names(prelim_temperature_all_hauls_df) <- tolower(names(prelim_temperature_all_hauls_df))
    
    temperature_all_hauls_df$preliminary <- FALSE
    prelim_temperature_all_hauls_df$preliminary <- TRUE
    
    temperature_all_hauls_df <- dplyr::bind_rows(temperature_all_hauls_df, prelim_temperature_all_hauls_df)
    
  }
  
  print("Writing temperature data for all hauls to csv")
  ifelse(!dir.exists(file.path(here::here("data"))), dir.create(file.path(here::here("data"))), FALSE)
  write.csv(temperature_all_hauls_df,
            file = here::here("data", paste0("ebs_nbs_temperature_full_area.csv")),
            row.names = FALSE)
  
  # Gear temperature for SEBS index stations
  temperature_df <- RODBC::sqlQuery(channel, coldpool::sql_to_rqry(system.file("sql", 
                                                                               "ebs_gear_temperature_cold_pool_hauls.sql", 
                                                                               package = "coldpool"))) %>%
    dplyr::mutate(LATITUDE = (START_LATITUDE + END_LATITUDE)/2,
                  LONGITUDE = (START_LONGITUDE + END_LONGITUDE)/2,
                  YEAR = floor(CRUISE/100)) %>%
    dplyr::select(-START_LATITUDE, 
                  -END_LATITUDE, 
                  -START_LONGITUDE, 
                  -END_LONGITUDE) %>%
    dplyr::filter(!is.na(GEAR_TEMPERATURE), !is.na(LATITUDE), !is.na(LONGITUDE))
  
  names(temperature_df) <- tolower(names(temperature_df))
  
  if(any(tolower(include_preliminary_data == "ebs"))) {
    prelim_temperature_df <- RODBC::sqlQuery(channel, 
                                             coldpool::sql_to_rqry(system.file("sql", "ebs_gear_temperature_cold_pool_hauls_current_year.sql", package = "coldpool"))) %>%
      dplyr::mutate(LATITUDE = coldpool::convert_ddm_to_dd(LATITUDE_DMS),
                    LONGITUDE = coldpool::convert_ddm_to_dd(LONGITUDE_DMS),
                    YEAR = floor(CRUISE/100)) %>%
      dplyr::select(-LATITUDE_DMS, -LONGITUDE_DMS, -CRUISE_ID, -EVENT_TYPE_ID, -SURVEY_DEFINITION_ID) %>%
      dplyr::filter(!is.na(GEAR_TEMPERATURE), !is.na(LATITUDE), !is.na(LONGITUDE))
    
    names(prelim_temperature_df) <- tolower(names(prelim_temperature_df))
    
    temperature_df$preliminary <- FALSE
    prelim_temperature_df$preliminary <- TRUE
    
    temperature_df <- dplyr::bind_rows(temperature_df, prelim_temperature_df)
    
  }
  
  print("Writing temperature data for index hauls to csv")
  write.csv(temperature_df,
            file = here::here("data", paste0("index_hauls_temperature_data.csv")),
            row.names = FALSE)
  
}
