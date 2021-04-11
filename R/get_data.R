# Get gear temperature data from all hauls, write to 'data/[date]_all_temperature_data.csv

get_data <- function(channel) {
  
  temperature_all_hauls_df <- sqlQuery(channel, "select gear_temperature,
         start_latitude,
         start_longitude,
         end_latitude,
         end_longitude,
         stationid,
         stratum,
         haul_type,
         performance,
         start_time,
         cruise
         from racebase.haul where
         region = 'BS' and
         (stratum in (10,20,31,32,41,42,43,50,61,62,82,90)) and 
         cruise > 198200 and
         bottom_depth < 201") %>%
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
  write.csv(temperature_all_hauls_df,
            file = here::here("data", paste0(Sys.Date(), "_all_temperature_data.csv")),
            row.names = FALSE)
  
  # Index hauls
  
  temperature_df <- sqlQuery(channel, "select gear_temperature,
         start_latitude,
         start_longitude,
         end_latitude,
         end_longitude,
         stationid,
         stratum,
         haul_type,
         performance,
         cruise
         from racebase.haul where
         region = 'BS' and
         (stratum in (10,20,31,32,41,42,43,50,61,62,82,90)) and 
         cruise > 198200 and
         bottom_depth < 201 and
         (haul_type in (3,13))") %>%
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
