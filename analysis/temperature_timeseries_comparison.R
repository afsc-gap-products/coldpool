library(akgfmaps)
library(ggrepel)
library(ggthemes)

ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")

esr_areas <- akgfmaps::get_esr_regions(select.region = "esr_subarea", set.crs = "EPSG:3338") |>
  dplyr::filter(AREA_NAME %in% c("Northern Bering Sea", "Southeastern Bering Sea"))

nbs_esr <- esr_areas |>
  dplyr::filter(AREA_NAME %in% c("Northern Bering Sea"))

sebs_nbsesr <- ebs_layers$survey.area |> 
  dplyr::filter(SURVEY_DEFINITION_ID == 98) |>
  dplyr::select(geometry) |>
  sf::st_intersection(nbs_esr) |>
  dplyr::mutate(AREA_M2 = as.numeric(sf::st_area(geometry)))

ggplot() +
  geom_sf(data = ebs_layers$akland) +
  geom_sf(data = esr_areas, fill = NA, 
          mapping = aes(color = "ESR Subareas"),
          linewidth = rel(1.5)) +
  geom_sf(data = sebs_nbsesr, mapping = aes(fill = "North of 60")) +
  geom_sf(data = ebs_layers$survey.area |> 
            dplyr::filter(SURVEY_DEFINITION_ID == 98) |>
            sf::st_intersection(dplyr::filter(esr_areas, AREA_NAME == "Southeastern Bering Sea")),
          mapping = aes(fill = "South of 60")) +
  geom_sf(data = ebs_layers$survey.area |>
            dplyr::inner_join(data.frame(SURVEY_DEFINITION_ID = 98,
                                         name = "Full EBS shelf")),
          mapping = aes(color = name),
          fill = NA,
          linewidth = rel(1.5)
  ) +
  scale_fill_colorblind() +
  scale_color_viridis_d() +
  scale_x_continuous(limits = ebs_layers$plot.boundary$x,
                     breaks = ebs_layers$lon.breaks) +
  scale_y_continuous(limits = ebs_layers$plot.boundary$y,
                     breaks = ebs_layers$lat.breaks) +
  theme_bw() +
  theme(legend.title = element_blank())


ggplot() +
  geom_sf(data = ebs_layers$akland) +
  geom_sf(data = sebs_nbsesr, 
          mapping = aes(fill = "North of 60")) +
  geom_sf(data = dplyr::filter(ebs_layers$survey.strata, SURVEY_DEFINITION_ID == 98),
          fill = NA) +
  geom_sf_text(data = dplyr::filter(ebs_layers$survey.strata, SURVEY_DEFINITION_ID == 98) |>
                 sf::st_centroid(),
               mapping = aes(label = STRATUM)) +
  scale_x_continuous(limits = ebs_layers$plot.boundary$x,
                     breaks = ebs_layers$lon.breaks) +
  scale_y_continuous(limits = ebs_layers$plot.boundary$y,
                     breaks = ebs_layers$lat.breaks) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title = element_blank())

sebs_nbsesr$AREA_M2 / ebs_layers$survey.area$AREA_M2[ebs_layers$survey.area$SURVEY_DEFINITION_ID == 98]

sebs_nbsesr$AREA_M2/1e6

sebs_bt <- terra::unwrap(coldpool::ebs_bottom_temperature)

bt_south_60 <- terra::mask(sebs_bt, 
                           esr_areas |>
                             dplyr::filter(AREA_NAME %in% c("Southeastern Bering Sea")))

bt_north_60 <- terra::mask(sebs_bt, 
                           sebs_nbsesr)

cpi <- coldpool::cold_pool_index

nbs_vs_sebs <- coldpool::nbs_mean_temperature |>
  dplyr::select(YEAR, NBS_MEAN_GEAR_TEMPERATURE = MEAN_GEAR_TEMPERATURE) |>
  dplyr::inner_join(cpi)

ggplot(data = nbs_vs_sebs,
       mapping = aes(x = MEAN_GEAR_TEMPERATURE, 
                     y = NBS_MEAN_GEAR_TEMPERATURE,
                     label = YEAR)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(name =  expression('BT ('*degree*C*'; BT Full EBS survey area)')) +
  scale_y_continuous(name =  expression('BT ('*degree*C*'; BT Full NBS survey area)')) +
  theme_bw() +
  ggtitle(paste0("EBS vs. NBS; r\U00B2 = " , round(cor(nbs_vs_sebs$MEAN_GEAR_TEMPERATURE, nbs_vs_sebs$NBS_MEAN_GEAR_TEMPERATURE)^2, 2)))

cor(nbs_vs_sebs$NBS_MEAN_GEAR_TEMPERATURE, nbs_vs_sebs$MEAN_GEAR_TEMPERATURE)^2

cpi$MEAN_GEAR_TEMPERATURE_N60 <- unlist(terra::global(bt_north_60, mean, na.rm = TRUE))

cpi$MEAN_GEAR_TEMPERATURE_S60 <- unlist(terra::global(bt_south_60, mean, na.rm = TRUE))


cor(cpi$MEAN_GEAR_TEMPERATURE_N60, cpi$MEAN_GEAR_TEMPERATURE_S60)^2
cor(cpi$MEAN_GEAR_TEMPERATURE_N60, cpi$MEAN_GEAR_TEMPERATURE)^2
cor(cpi$MEAN_GEAR_TEMPERATURE_S60, cpi$MEAN_GEAR_TEMPERATURE)^2


cowplot::plot_grid(
  ggplot(data = cpi,
         mapping = 
           aes(x = MEAN_GEAR_TEMPERATURE, 
               y = MEAN_GEAR_TEMPERATURE_N60,
               label = YEAR)) +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous(name = expression('BT ('*degree*C*'; Full survey area)')) +
    scale_y_continuous(name = expression('BT ('*degree*C*'; North of 60'*degree*N*')')) +
    ggtitle(paste0("Full survey area vs. North of 60N; r\U00B2 = " , round(cor(cpi$MEAN_GEAR_TEMPERATURE_N60, cpi$MEAN_GEAR_TEMPERATURE)^2, 2))) +
    theme_bw(),
  ggplot(data = cpi,
         mapping = 
           aes(x = MEAN_GEAR_TEMPERATURE, 
               y = MEAN_GEAR_TEMPERATURE_S60,
               label = YEAR)) +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous(name = expression('BT ('*degree*C*'; Full survey area)')) +
    scale_y_continuous(name = expression('BT ('*degree*C*'; South of 60'*degree*N*')')) +
    ggtitle(paste0("Full survey area vs. South of 60N; r\U00B2 = " , round(cor(cpi$MEAN_GEAR_TEMPERATURE_S60, cpi$MEAN_GEAR_TEMPERATURE)^2, 2))) +
    theme_bw(),
  ggplot(data = cpi,
         mapping = 
           aes(x = MEAN_GEAR_TEMPERATURE_S60, 
               y = MEAN_GEAR_TEMPERATURE_N60,
               label = YEAR)) +
    geom_point() +
    geom_text_repel() +
    scale_x_continuous(name = expression('BT ('*degree*C*'; South of 60'*degree*N*')')) +
    scale_y_continuous(name = expression('BT ('*degree*C*'; North of 60'*degree*N*')')) +
    ggtitle(paste0("South of 60N vs. North of 60N; r\U00B2 = " , round(cor(cpi$MEAN_GEAR_TEMPERATURE_S60, cpi$MEAN_GEAR_TEMPERATURE_N60)^2, 2))) +
    theme_bw()
)

