#' Map survey stations and regions w/ sampling day of year
#' 
#' Make a plot showing survey stations and sampling day of year
#' 
#' @noRd

plot_stn_doy <- function() {
  
  channel <- coldpool::get_connected(schema = "AFSC")
  
  # Retrieve haul data from RACEBASE
  qry_haul <- "select a.* from RACEBASE.HAUL a, RACE_DATA.V_CRUISES b where a.cruisejoin = b.cruisejoin and b.survey_definition_id in (98, 143) and a.region = 'BS' and a.abundance_haul = 'Y' and a.cruise > 198200 and a.performance >=0 and a.haul_type = 3"
  
  # Day of year of hauls
  haul_dat <- RODBC::sqlQuery(channel = channel, 
                              query = qry_haul) |>
    dplyr::filter(STATIONID %in% akgfmaps::get_survey_stations(select.region = "ebs")) |>
    dplyr::mutate(DOY = yday(as.POSIXct(START_TIME)))
  
  # Retrieve EBS base layers
  ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "EPSG:3338")
  
  # Join with survey grid and calculate mean day of year grid cells were sampled
  start_df <- ebs_layers$survey.grid |> 
    sf::st_intersection(ebs_layers$survey.area |>
                          dplyr::mutate(REGION = "EBS") |>
                          dplyr::group_by(REGION) |>
                          dplyr::summarise()) |>
    dplyr::inner_join(haul_dat |>
                        dplyr::group_by(STATIONID) |>
                        dplyr::summarise(MEAN_DOY = mean(DOY),
                                         MEAN_DEPTH = mean(BOTTOM_DEPTH, na.rm = TRUE)))
  
  # Define plot exent (through trial end error)
  panel_extent <- data.frame(x = c(-1326559.21, -87636.05),
                             y = c(533099.5, 1894909.7))
  
  # Combine strata by main survey region + SEBS plus NW
  agg_stratum <- ebs_layers$survey.strata |>
    dplyr::mutate(agg_stratum = Stratum) |>
    dplyr::mutate(agg_stratum = replace(agg_stratum, agg_stratum %in% c(31,32,30,41,42,43,61,62,60,10,20,50), "EBS Shelf\nStandard"),
                  agg_stratum = replace(agg_stratum, agg_stratum %in% c(82, 90), "EBS\nPlus NW"),
                  agg_stratum = replace(agg_stratum, agg_stratum %in% c(70,71,81), "NBS")) |> 
    dplyr::group_by(agg_stratum) |> 
    dplyr::summarise()
  
  plot_ebs_nbs_survey_stations <- ggplot() +
    geom_sf(data = ebs_layers$akland, 
            fill = "grey70", 
            color = "black") +
    geom_sf(data = start_df, 
            aes(fill = MEAN_DOY)) +
    ggplot2::geom_sf(data = agg_stratum,
                     fill = NA,
                     color = "black",
                     size = rel(1.1)) +
    shadowtext::geom_shadowtext(data = data.frame(agg_stratum = sf::st_centroid(agg_stratum)$agg_stratum, # Centroid of aggregate stratum polygons
                                                  x = sf::st_coordinates(sf::st_centroid(agg_stratum))[,1], # Coordinates of centroid of aggregate stratum polygons
                                                  y = sf::st_coordinates(sf::st_centroid(agg_stratum))[,2]),# Coordinates of centroid of aggregate stratum polygons 
                                aes(x = x,
                                    y = y, 
                                    label = agg_stratum),
                                size = rel(4.5),
                                color = "black",
                                bg.color = "white") +
    shadowtext::geom_shadowtext(data = data.frame(x = -158.5, 
                                                  y = 62.4, 
                                                  lab = "Alaska") |>
                                  akgfmaps::transform_data_frame_crs(out.crs = coldpool:::ebs_proj_crs),
                                mapping = aes(x = x,
                                              y = y,
                                              label = lab),
                                size = rel(6),
                                color = "black",
                                bg.color = "white") +
    shadowtext::geom_shadowtext(data = data.frame(x = -166.2, 
                                                  y = 60.08, 
                                                  lab = "Nunivak\nIsland") |>
                                  akgfmaps::transform_data_frame_crs(out.crs = coldpool:::ebs_proj_crs),
                                mapping = aes(x = x,
                                              y = y,
                                              label = lab),
                                size = rel(2.5),
                                color = "black",
                                bg.color = "white") +
    shadowtext::geom_shadowtext(data = data.frame(x = c(-169, -159.5), 
                                                  y = c(66.3, 57.7), 
                                                  lab = c("Bering\nStrait", "Bristol\nBay")) |>
                                  akgfmaps::transform_data_frame_crs(out.crs = coldpool:::ebs_proj_crs),
                                mapping = aes(x = x,
                                              y = y,
                                              label = lab),
                                size = rel(3),
                                color = "black",
                                bg.color = "white") +
    scale_fill_viridis_c(name = "Sample Mean\nDay of Year", 
                         option = "B") +
    scale_color_brewer() +
    coord_sf(xlim = panel_extent$x,
             ylim = panel_extent$y) +
    ggplot2::scale_x_continuous(name = "Longitude", 
                                breaks = ebs_layers$lon.breaks) + 
    ggplot2::scale_y_continuous(name = "Latitude", 
                                breaks = ebs_layers$lat.breaks) +
    theme_bw() +
    ggplot2::theme(axis.title = element_blank(),
                   panel.border = element_rect(color = "black", fill = NA),
                   legend.title = element_text(size = 9, color = "black"),
                   legend.text = element_text(size = 8, color = "black"),
                   axis.text = element_text(size = 8, color = "black"),
                   panel.grid = element_blank(),
                   panel.background = element_rect(color = "black", fill = "#bee8ff"),
                   legend.margin = margin(-12,0,0,0),
                   legend.position = "right",
                   legend.background = element_blank())
  
  doy_for_tm <- ggplot() +
    geom_sf(data = ebs_layers$akland, 
            fill = "grey70", 
            color = "black") +
    geom_sf(data = start_df, 
            aes(fill = MEAN_DOY)) +
    geom_sf_text(data = start_df, 
            aes(label = round(MEAN_DOY))) +
    ggplot2::geom_sf(data = agg_stratum,
                     fill = NA,
                     color = "black",
                     size = rel(1.1)) +
    shadowtext::geom_shadowtext(data = data.frame(agg_stratum = sf::st_centroid(agg_stratum)$agg_stratum, # Centroid of aggregate stratum polygons
                                                  x = sf::st_coordinates(sf::st_centroid(agg_stratum))[,1], # Coordinates of centroid of aggregate stratum polygons
                                                  y = sf::st_coordinates(sf::st_centroid(agg_stratum))[,2]),# Coordinates of centroid of aggregate stratum polygons 
                                aes(x = x,
                                    y = y, 
                                    label = agg_stratum),
                                size = rel(4.5),
                                color = "black",
                                bg.color = "white") +
    shadowtext::geom_shadowtext(data = data.frame(x = -158.5, 
                                                  y = 62.4, 
                                                  lab = "Alaska") |>
                                  akgfmaps::transform_data_frame_crs(out.crs = coldpool:::ebs_proj_crs),
                                mapping = aes(x = x,
                                              y = y,
                                              label = lab),
                                size = rel(6),
                                color = "black",
                                bg.color = "white") +
    shadowtext::geom_shadowtext(data = data.frame(x = -166.2, 
                                                  y = 60.08, 
                                                  lab = "Nunivak\nIsland") |>
                                  akgfmaps::transform_data_frame_crs(out.crs = coldpool:::ebs_proj_crs),
                                mapping = aes(x = x,
                                              y = y,
                                              label = lab),
                                size = rel(2.5),
                                color = "black",
                                bg.color = "white") +
    shadowtext::geom_shadowtext(data = data.frame(x = c(-169, -159.5), 
                                                  y = c(66.3, 57.7), 
                                                  lab = c("Bering\nStrait", "Bristol\nBay")) |>
                                  akgfmaps::transform_data_frame_crs(out.crs = coldpool:::ebs_proj_crs),
                                mapping = aes(x = x,
                                              y = y,
                                              label = lab),
                                size = rel(3),
                                color = "black",
                                bg.color = "white") +
    scale_fill_viridis_c(name = "Sample Mean\nDay of Year", 
                         option = "B") +
    scale_color_brewer() +
    coord_sf(xlim = panel_extent$x,
             ylim = panel_extent$y) +
    ggplot2::scale_x_continuous(name = "Longitude", 
                                breaks = ebs_layers$lon.breaks) + 
    ggplot2::scale_y_continuous(name = "Latitude", 
                                breaks = ebs_layers$lat.breaks) +
    theme_bw() +
    ggplot2::theme(axis.title = element_blank(),
                   panel.border = element_rect(color = "black", fill = NA),
                   legend.title = element_text(size = 9, color = "black"),
                   legend.text = element_text(size = 8, color = "black"),
                   axis.text = element_text(size = 8, color = "black"),
                   panel.grid = element_blank(),
                   panel.background = element_rect(color = "black", fill = "#bee8ff"),
                   legend.margin = margin(-12,0,0,0),
                   legend.position = "right",
                   legend.background = element_blank())
  
  png(filename = here::here("plots", "ebs_nbs_survey_area.png"), width = 5, height = 5, units = "in", res = 600)
  print(plot_ebs_nbs_survey_stations)
  dev.off()
  
  png(filename = here::here("plots", "doy_for_tm.png"), width = 10, height = 10, units = "in", res = 120)
  print(doy_for_tm)
  dev.off()
  
}
