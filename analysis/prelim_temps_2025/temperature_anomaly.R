library(coldpool)
library(akgfmaps)

sebs_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "EPSG:3338")

data_2025 <- read.csv(file = here::here("analysis", "prelim_temps_2025", "ebs_temps_2025.csv"), header = TRUE) |>
  dplyr::rename(stationid = station, gear_temperature = bt) |>
  dplyr::filter(!is.na(gear_temperature)) |>
  dplyr::mutate(year = 2025) |>
  dplyr::select(stationid, year, gear_temperature)

data_historical <- read.csv(file = here::here("assets", "index_hauls_temperature_data.csv")) |>
  dplyr::filter(stationid %in% data_2025$stationid, haul_type != 24)

data <- dplyr::bind_rows(data_historical, data_2025)

ggplot() +
  geom_boxplot(data = data_historical ,
             mapping = aes(x = stationid, y = gear_temperature)) +
  geom_point(data = data_2025, 
             mapping = aes(x = stationid, y = gear_temperature, color = "2025")) +
  theme(axis.text = element_text(angle = 90, vjust = 0))


ggplot() +
  geom_point(data = data,
               mapping = aes(x = stationid, y = gear_temperature, color = year == 2025))

bt_anomaly <- 
data |>
  dplyr::group_by(stationid) |>
  dplyr::summarise(mean_bt = mean(gear_temperature, na.rm = TRUE),
                   sd_bt = sd(gear_temperature, na.rm = TRUE)) |>
  dplyr::inner_join(
    data_2025 |>
      dplyr::select(stationid, gear_temperature)
  ) |>
  dplyr::mutate(anomaly_bt = (gear_temperature-mean_bt)/sd_bt) |>
  dplyr::rename(STATION = stationid)


bt_anomaly_sf <-
  dplyr::left_join(
    sebs_layers$survey.grid,
    bt_anomaly
  )

ggplot() +
  geom_sf(data = bt_anomaly_sf,
          mapping = aes(fill = cut(anomaly_bt, seq(-1, 2, 0.5)))) +
  scale_fill_viridis_d(name = "Temperature anomaly", option = "H", na.value = NA) +
  theme_bw()


ggplot() +
  geom_sf(data = bt_anomaly_sf,
          mapping = aes(fill = cut(anomaly_bt, seq(-1, 2, 0.5)))) +
  geom_sf_text(data = sf::st_centroid(sebs_layers$survey.grid), mapping = aes(label = STATION)) +
  scale_fill_viridis_d(name = "Temperature anomaly", option = "H", na.value = NA) +
  theme_bw()
