# Dissolved Oxygen maps for September 2024 GPT

library(RNetCDF)
library(ncdf4)
library(coldpool)
library(stars)
library(akgfmaps)

dir.create(here::here("analysis", 
                      "dissolved_oxygen_2024",
                      "plots"),
           showWarnings = FALSE)

fig_res <- 300

do_breaks <- c(-Inf, 3:9, Inf)

map_layers_2023 <- akgfmaps::get_base_layers(select.region = "ebs", 
                                             set.crs = "EPSG:3338")

map_layers_2024 <- akgfmaps::get_base_layers(select.region = "sebs", 
                                             set.crs = "EPSG:3338")

con <- ncdf4::nc_open(here::here("analysis", "dissolved_oxygen_2024", "data", "GAPCTD_2023_EBS.nc"))

do_df <- data.frame(sea_floor_dissolved_oxygen = ncvar_get(con, "sea_floor_dissolved_oxygen"),
           latitude = ncvar_get(con, "latitude"),
           longitude = ncvar_get(con, "longitude"),
           stationid = ncvar_get(con, "stationid"),
           year = 2023)



con <- ncdf4::nc_open(here::here("analysis", "dissolved_oxygen_2024", "data", "GAPCTD_2024_EBS.nc"))

do_df <- dplyr::bind_rows(do_df,
                          data.frame(sea_floor_dissolved_oxygen = ncvar_get(con, "sea_floor_dissolved_oxygen"),
                                     latitude = ncvar_get(con, "latitude"),
                                     longitude = ncvar_get(con, "longitude"),
                                     stationid = ncvar_get(con, "stationid"),
                                     year = 2024))

do_df <- do_df |>
  dplyr::filter(sea_floor_dissolved_oxygen < 1000)


do_2023 <- do_df |>
  dplyr::filter(year == 2023) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338")

do_2024 <- do_df |>
  dplyr::filter(year == 2024) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338")

do_2024_slope <- do_2024 |>
  dplyr::filter(!(stationid %in% akgfmaps::get_survey_stations(select.region = "sebs")))


do_2024_sf <- coldpool::interpolate_variable(dat = dplyr::filter(do_sf, year == 2024), 
                                          dat.year = 2024, 
                                          select.region = "sebs",
                                          lat.col = "latitude",
                                          lon.col = "longitude",
                                          var.col = "sea_floor_dissolved_oxygen",
                                          in.crs = "WGS84",
                                          interpolation.crs = "EPSG:3338", 
                                          cell.resolution = 5000,
                                          methods = "Ste",
                                          return_raster = TRUE) |>
  terra::mask(map_layers_2024$survey.area) |>
  terra::trim() |>
  terra::as.polygons() |>
  sf::st_as_sf() |>
  sf::st_intersection(map_layers_2024$survey.area)

do_2023_sf <- coldpool::interpolate_variable(dat = dplyr::filter(do_sf, year == 2023), 
                                             dat.year = 2023, 
                                             select.region = "sebs",
                                             lat.col = "latitude",
                                             lon.col = "longitude",
                                             var.col = "sea_floor_dissolved_oxygen",
                                             in.crs = "WGS84",
                                             interpolation.crs = "EPSG:3338", 
                                             cell.resolution = 5000,
                                             methods = "Ste",
                                             return_raster = TRUE) |>
  terra::mask(map_layers_2023$survey.area) |>
  terra::trim() |>
  terra::as.polygons() |>
  sf::st_as_sf() |>
  sf::st_intersection(map_layers_2023$survey.area)

map_2023 <- ggplot() +
  geom_sf(data = do_2023,
          mapping = aes(color = cut(sea_floor_dissolved_oxygen, breaks = do_breaks, right = TRUE)), 
          size = 1.7) +
  geom_sf(data = map_layers_2023$akland) +
  geom_sf(data = map_layers_2023$survey.strata, fill = NA) +
  ggtitle(label = 2023) +
  coord_sf(xlim = map_layers_2023$plot.boundary$x,
           ylim = map_layers_2023$plot.boundary$y) +
  scale_x_continuous(breaks = map_layers_2023$lon.breaks) +
  scale_y_continuous(breaks = map_layers_2023$lat.breaks) +
  scale_color_viridis_d(option = "rocket",
                       direction = -1,
                       na.value = NA,
                       drop = FALSE) +
  coldpool::theme_multi_map_blue_strip() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(5,5,-5,5), units = "mm"),
        axis.title = element_blank(),
        axis.text = element_text(size = 9))

map_2023_raster <- ggplot() +
  geom_sf(data = do_2023_sf,
          mapping = aes(fill = cut(lyr.1, breaks = do_breaks, right = TRUE)),
          color = NA) +
  geom_sf(data = map_layers_2023$akland) +
  geom_sf(data = map_layers_2023$survey.strata, fill = NA) +
  ggtitle(label = 2023) +
  coord_sf(xlim = map_layers_2023$plot.boundary$x,
           ylim = map_layers_2023$plot.boundary$y) +
  scale_x_continuous(breaks = map_layers_2023$lon.breaks) +
  scale_y_continuous(breaks = map_layers_2023$lat.breaks) +
  scale_fill_viridis_d(option = "rocket",
                       direction = -1,
                       na.value = NA,
                       drop = FALSE) +
  coldpool::theme_multi_map_blue_strip() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(5,5,-5,5), units = "mm"),
        axis.title = element_blank(),
        axis.text = element_text(size = 9))

map_2023_raster

map_2024 <- ggplot() +
  geom_sf(data = do_2024_sf,
          mapping = aes(fill = cut(lyr.1, breaks = do_breaks, right = TRUE)),
          color = NA) +
  geom_sf(data = map_layers$survey.grid |> 
            sf::st_centroid(),
          shape = 4,
          size = 0.5) +
  geom_sf(data = do_2024_slope,
          mapping = aes(fill = cut(sea_floor_dissolved_oxygen, 
                                   breaks = do_breaks, 
                                   right = TRUE)),
          shape = 21,
          size = 1.7) +
  geom_sf(data = map_layers_2023$akland) +
  geom_sf(data = map_layers_2023$survey.strata, fill = NA) +
  ggtitle(label = 2024) +
  coord_sf(xlim = map_layers_2023$plot.boundary$x,
           ylim = map_layers_2023$plot.boundary$y) +
  scale_x_continuous(breaks = map_layers_2023$lon.breaks) +
  scale_y_continuous(breaks = map_layers_2023$lat.breaks) +
  scale_fill_viridis_d(option = "rocket",
                       direction = -1,
                       na.value = NA,
                       drop = FALSE,
                       guide = "none") +
  coldpool::theme_multi_map_blue_strip() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(5,5,-5,5), units = "mm"),
        axis.title = element_blank(),
        axis.text = element_text(size = 9))

cbar_legend <- coldpool::legend_discrete_cbar(breaks = do_breaks,
                                                colors = viridis::viridis_pal(option = "rocket",
                                                                              direction = -1),
                                                legend_direction = "horizontal",
                                                font_size = 3,
                                                width = 0.1,
                                                expand_size.x = 0.3,
                                                expand_size.y = 0.3,
                                                expand.x = 0.3,
                                                expand.y = 0.9,
                                                spacing_scaling = 1,
                                                text.hjust = 0.5,
                                                font.family = "sans",
                                                neat.labels = FALSE) + 
  annotate("text", 
           x = 1.25, 
           y = 5.5, 
           label = expression('Bottom Dissolved Oxygen ('*ml%.%l^-1*')'), 
           size = rel(3.2)) + 
  theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))


do_map_grid <- cowplot::plot_grid(cowplot::plot_grid(map_2023, map_2024, align = "hv"), 
                   cbar_legend,
                   nrow = 2, 
                   ncol = 1,
                   rel_heights = c(0.85, 0.25))


ragg::agg_png(filename = here::here("analysis", 
                                    "dissolved_oxygen_2024",
                                    "plots",
                                    paste0("2024_dissolved_oxygen_map.png")), width = 6, height = 4, units = "in", res = fig_res)
print(do_map_grid)
dev.off()

