# Dissolved Oxygen maps for September 2024 GPT

library(RNetCDF)
library(ncdf4)
library(coldpool)
library(stars)
library(akgfmaps)
library(tidyterra)

dir.create(here::here("analysis", 
                      "dissolved_oxygen",
                      "plots"),
           showWarnings = FALSE)

fig_res <- 300

grid_res <- c(5000, 5000)

sebs_years <- c(2024, 2026)
full_ebs_years <- 2023


# do_breaks <- c(-Inf, seq(2,10,2), Inf)
do_breaks <- c(-Inf, 3:9, Inf)

do_rcl <- cbind(
  do_breaks[1:(length(do_breaks)-1)], 
  do_breaks[2:(length(do_breaks))], 
  1:(length(do_breaks)-1))

do_class <-
  data.frame(
    lyr.1 = 1:(length(do_breaks)-1),
    level = cut(do_breaks, breaks = do_breaks)[2:length(do_breaks)]
  )

map_layers_2023 <- akgfmaps::get_base_layers(select.region = "ebs", 
                                             set.crs = "EPSG:3338")

map_layers_2024 <- akgfmaps::get_base_layers(select.region = "sebs", 
                                             set.crs = "EPSG:3338")

con <- ncdf4::nc_open(here::here("analysis", "dissolved_oxygen", "data", "GAPCTD_2023_EBS.nc"))

do_df <- data.frame(sea_floor_dissolved_oxygen = ncvar_get(con, "sea_floor_dissolved_oxygen"),
           latitude = ncvar_get(con, "latitude"),
           longitude = ncvar_get(con, "longitude"),
           stationid = ncvar_get(con, "stationid"),
           year = 2023)



con <- ncdf4::nc_open(here::here("analysis", "dissolved_oxygen", "data", "GAPCTD_2024_EBS.nc"))

do_df <- dplyr::bind_rows(do_df,
                          data.frame(sea_floor_dissolved_oxygen = ncvar_get(con, "sea_floor_dissolved_oxygen"),
                                     latitude = ncvar_get(con, "latitude"),
                                     longitude = ncvar_get(con, "longitude"),
                                     stationid = ncvar_get(con, "stationid"),
                                     year = 2024))

con <- ncdf4::nc_open(here::here("analysis", "dissolved_oxygen", "data", "GAPCTD_2026_EBS.nc"))

do_df <- dplyr::bind_rows(do_df,
                          data.frame(sea_floor_dissolved_oxygen = ncvar_get(con, "sea_floor_dissolved_oxygen"),
                                     latitude = ncvar_get(con, "latitude"),
                                     longitude = ncvar_get(con, "longitude"),
                                     stationid = ncvar_get(con, "stationid"),
                                     year = 2026))

do_df <- do_df |>
  dplyr::filter(sea_floor_dissolved_oxygen < 1000)


do_2023 <- do_df |>
  dplyr::filter(year == 2023) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338") |>
  dplyr::mutate(level = cut(sea_floor_dissolved_oxygen, breaks = do_breaks, right = TRUE))

do_sebs <- do_df |>
  dplyr::filter(year %in% c(2024, 2026)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338")

do_sebs_slope <- do_sebs |>
  dplyr::filter(!(stationid %in% akgfmaps::get_survey_stations(select.region = "sebs"))) |>
  dplyr::mutate(level = cut(sea_floor_dissolved_oxygen, breaks = do_breaks, right = TRUE))

for(ii in 1:length(sebs_years)) {
  
  new_sebs_layer <- coldpool::interpolate_variable(
    dat = dplyr::filter(do_sebs, year == sebs_years[ii]), 
    dat.year = sebs_years[ii], 
    select.region = "sebs",
    lat.col = "latitude",
    lon.col = "longitude",
    var.col = "sea_floor_dissolved_oxygen",
    in.crs = "WGS84",
    interpolation.crs = "EPSG:3338", 
    cell.resolution = grid_res,
    methods = "ste",
    return_raster = TRUE
  ) |>
    terra::mask(map_layers_2024$survey.area) |>
    terra::trim()
  
  new_sebs_sf <- 
    new_sebs_layer |>
    terra::classify(do_rcl) |>
    terra::as.polygons() |>
    sf::st_as_sf() |>
    sf::st_intersection(map_layers_2024$survey.area) |>
    dplyr::mutate(year = sebs_years[ii])
  
  if(ii == 1) {
    do_rast <- new_sebs_layer
    do_sf <- new_sebs_sf
  } else {
    do_rast <- c(do_rast, new_sebs_layer)
    do_sf <- dplyr::bind_rows(do_sf, new_sebs_sf)
  }
  
}

names(do_sebs_rast) <- sebs_years

do_sf <- dplyr::inner_join(
  do_sf, do_class
)

all_do_sf <- do_sf |> 
  dplyr::bind_rows(do_2023) |>
  dplyr::bind_rows(do_sebs_slope)

stns <- map_layers_2024$survey.grid |> 
  sf::st_centroid()

stns <- dplyr::bind_rows(
  dplyr::mutate(stns, year = 2024),
  dplyr::mutate(stns, year = 2026)
)

p_do_map <- ggplot() +
  geom_sf(data = dplyr::filter(all_do_sf, sf::st_geometry_type(geometry) != "POINT"),
          mapping = aes(fill = level), color = NA) +
  geom_sf(data = dplyr::filter(all_do_sf, sf::st_geometry_type(geometry) == "POINT"),
          mapping = aes(fill = level), shape = 21, size = 1.7) +
  # geom_sf(data = stns,
  #         shape = 4,
  #         size = 0.3) +
  geom_sf(data = map_layers_2023$akland) +
  geom_sf(data = map_layers_2023$survey.strata, fill = NA) +
  facet_wrap(~year) +
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
           label = "Bottom Dissolved Oxygen (ml\u00B7l\u207B\u00B9)", 
           size = rel(3.2),
           fontface = "bold") + 
  theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))


do_map_grid <- 
  cowplot::plot_grid(
    p_do_map, 
    cbar_legend,
    nrow = 2, 
    ncol = 1,
    rel_heights = c(0.85, 0.25)
  )


ragg::agg_png(
  filename = here::here("analysis", 
                        "dissolved_oxygen",
                        "plots",
                        paste0("2026_dissolved_oxygen_map.png")), 
  width = 8, 
  height = 4, 
  units = "in", 
  res = fig_res)
print(do_map_grid)
dev.off()

