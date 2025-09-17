library(coldpool)
library(plotly)

channel <- coldpool::get_connected(schema = "AFSC")

nbs_dat <-
  RODBC::sqlQuery(
    channel = channel,
    query = "select e.gear_temperature,
    e.haul_id,
e.surface_temperature,
f.latitude,
f.longitude,
e.station stationid,
e.stratum,
e.haul,
e.haul_type,
e.performance,
e.cruise_id,
e.bottom_depth bottom_depth,
c.cruise,
c.vessel_id vessel,
f.event_type_id,
a.survey_definition_id
from race_data.hauls e,
race_data.survey_definitions a, 
race_data.surveys b,
race_data.cruises c,
race_data.events f
where b.survey_definition_id = 143
and  c.cruise = 202502
and e.cruise_id = c.cruise_id 
and a.survey_definition_id = b.survey_definition_id
and b.survey_id = c.survey_id
and e.performance >= 0 
and e.bottom_depth < 201
and (e.haul_type in (3, 13))
and f.haul_id = e.haul_id
and f.event_type_id = 9"
  ) |>
  sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338")

map_layers <- akgfmaps::get_base_layers(select.region = "nbs", set.crs = "EPSG:3338")

haul_grid <- sf::st_join(dplyr::select(map_layers$survey.grid, -STRATUM, -SURVEY_DEFINITION_ID), nbs_dat)

ggplot() +
  geom_sf(data = map_layers$survey.grid, fill = NA) +
  geom_sf(data = haul_grid,
          mapping = aes(fill = SURFACE_TEMPERATURE
                          )) +
  geom_sf(data = sf::st_centroid(nbs_dat),
               mapping = aes(shape = factor(VESSEL), color = factor(VESSEL)), size = rel(2)) +
  geom_sf_text(data = sf::st_centroid(nbs_dat),
               mapping = aes(label = STATIONID),
               size = rel(2.5)) +
  scale_color_tableau() +
  scale_fill_viridis_c(option = "H")

# Five stations have unusual surface temperatures
check_hauls <- nbs_dat |>
  dplyr::filter(STATIONID %in% c("BB-06", "BB-03", "CC-02", "AA-02", "ZZ-01"))

check_casts <-
  RODBC::sqlQuery(
    channel = channel,
    query = "select 
c.vessel_id as vessel, 
                c.cruise, 
                h.haul, 
                h.station stationid,
                bt.date_time,
                bt.depth,
                bt.temperature
              from 
                race_data.surveys s,
                race_data.cruises c,
                race_data.hauls h,
                race_data.bathythermic_headers bh, 
                race_data.bathythermics bt
              where s.survey_definition_id = 143 
              and s.survey_id = c.survey_id
                and c.cruise_id = h.cruise_id
                and h.abundance_haul = 'Y'
                and h.haul_id = bh.haul_id
                and bh.bathythermic_header_id = bt.bathythermic_header_id 
                and bt.datum_code in (0, 1, 7, 11)
                and bt.depth >= 0 
                and c.cruise = 202502 
                and c.vessel_id = 162 
and h.haul in (29, 38, 40, 42, 54)"
  )



tidyr::pivot_longer(cols = c("DEPTH", "TEMPERATURE"))

plotly::ggplotly(
ggplot() +
  geom_point(data = check_casts,
             mapping = aes(y = DEPTH, x = TEMPERATURE)) +
  geom_vline(data = check_hauls,
             mapping = aes(xintercept = SURFACE_TEMPERATURE)
             ) +
  scale_y_reverse() +
  facet_wrap(~STATIONID, scales = "free")
)

plotly::ggplotly(
  ggplot() +
    geom_point(data = dplyr::filter(check_casts),
               mapping = aes(x = DATE_TIME, y = TEMPERATURE, label = DEPTH)) +
    geom_hline(data = check_hauls,
               mapping = aes(yintercept = SURFACE_TEMPERATURE)
    ) +
    facet_wrap(~STATIONID, scales = "free")
)

plotly::ggplotly(
  ggplot() +
    geom_point(data = dplyr::filter(check_casts, DEPTH < 16),
               mapping = aes(x = DATE_TIME, y = TEMPERATURE, label = DEPTH)) +
    geom_hline(data = check_hauls,
               mapping = aes(yintercept = SURFACE_TEMPERATURE)
    ) +
    facet_wrap(~STATIONID, scales = "free")
)


ggplot() +
  geom_point(data = dplyr::filter(check_casts),
             mapping = aes(x = DATE_TIME, y = TEMPERATURE, label = DEPTH)) +
  geom_hline(data = check_hauls,
             mapping = aes(yintercept = SURFACE_TEMPERATURE)
  ) +
  facet_wrap(~STATIONID, scales = "free")


recommended <- data.frame(
  STATIONID = c("AA-02", "BB-03", "BB-06", "CC-02", "ZZ-01"),
  SURFACE_TEMPERATURE = c(7.8, 9.8, NA, 10.2, 5.9)
)

ggplot() +
  geom_point(data = dplyr::filter(check_casts, DEPTH <= 10),
             mapping = aes(y = DEPTH, x = TEMPERATURE)) +
  geom_vline(data = check_hauls,
             mapping = aes(xintercept = SURFACE_TEMPERATURE, color = "Current")
  ) +
  geom_vline(data = recommended ,
             mapping = aes(xintercept = SURFACE_TEMPERATURE, color = "Recommended")
  ) +
  scale_y_reverse(limits = c(10, 0), breaks = 1:10) +
  facet_wrap(~STATIONID, scales = "free_x") +
  theme_bw()

write.csv(check_hauls, "check_nbs_sst.csv", row.names = FALSE)
