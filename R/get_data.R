#' Get gear temperature data from all hauls, write to 'data/[date]_all_temperature_data.csv
#' 
#' @param channel ODBC connection as an RODBC class
#' @export

get_data <- function(channel) {
    
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
  
  print("Writing temperature data for all hauls to csv")
  ifelse(!dir.exists(file.path(here::here("data"))), dir.create(file.path(here::here("data"))), FALSE)
  write.csv(temperature_all_hauls_df,
            file = here::here("data", paste0(Sys.Date(), "_ebs_nbs_temperature_full_area.csv")),
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
  
  print("Writing temperature data for index hauls to csv")
  write.csv(temperature_df,
            file = here::here("data", paste0(Sys.Date(), "_index_hauls_temperature_data.csv")),
            row.names = FALSE)
  
}
