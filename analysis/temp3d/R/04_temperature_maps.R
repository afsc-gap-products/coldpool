# Explore plotting options
library(coldpool)
library(tidyterra)
library(spmodel)

survey_definition_id <- 47

fig_res <- 300
sel_year <- 2025

# Setup
if(all(survey_definition_id == 47)) {
  utmcrs <- "EPSG:32605"
  region <- "GOA"
  min_year <- 1993
  range_baseline <- c(1993, 2014)
  subarea_levels <- c("Western Gulf of Alaska", "Eastern Gulf of Alaska") # Panel/timeseries order
}

if(all(survey_definition_id == 52)) {
  utmcrs <- "EPSG:32660"
  region <- "AI"
  min_year <- 1991
  range_baseline <- c(1991, 2012)
  subarea_levels <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians") # Panel/timeseries order
}

if(all(survey_definition_id == 98)) {
  utmcrs <- "EPSG:32602"
  region <- "SEBS"
  min_year <- 1982
}

if(all(survey_definition_id == 143)) {
  utmcrs <- "EPSG:32602"
  region <- "NBS"
  min_year <- 2010
}

if(all(survey_definition_id %in% c(143, 98))) {
  utmcrs <- "EPSG:32602"
  region <- "EBS"
  min_year <- 1982
}

# Function to select the best fit model for interpolation
map_layers <- 
  akgfmaps::get_base_layers(
    select.region = "goa", 
    set.crs = coldpool::ebs_proj_crs
  )

esr_subareas <-
  akgfmaps::get_esr_regions(select.region = "esr_subarea", set.crs = "EPSG:3338") |>
  dplyr::filter(AREA_NAME %in% subarea_levels)

# Load bathymetry raster, mask to survey extent, trim whitespace, convert to sf, change depth column name to match model
bathy <- 
  system.file("extdata", "bathymetry.gpkg", package = "akgfmaps") |>
  terra::rast() |>
  terra::mask(map_layers$survey.area) |>
  terra::trim() 

bt <- readRDS(here::here("output", paste0(region, "_bt.rds")))

bt[["2001"]] <- mask(
  bt["2001"], 
  esr_subareas[esr_subareas$AREA_NAME == "Eastern Gulf of Alaska", ],
  inverse = TRUE
)

haul_data <- readRDS(here::here("data", region, paste0(region, "_akfin_haul.rds"))) |>
  sf::st_as_sf(coords = c("LONGITUDE_DD_START", "LATITUDE_DD_START"), crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338")

ggplot() +
  geom_spatraster(data = bt["2001"]) +
  geom_sf(data = dplyr::filter(haul_data, YEAR == 2001))

ggplot() +
  geom_spatraster(data = bt) +
  scale_fill_viridis_c(option = "H", na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  geom_spatraster(data = c(bt-mean(bt, na.rm = TRUE))/stdev(bt, na.rm = TRUE)) +
  scale_fill_viridis_c(option = "H", na.value = NA) +
  facet_wrap(~lyr)

ggplot() +
  geom_spatraster(data = c(bt-mean(bt, na.rm = TRUE))/stdev(bt, na.rm = TRUE)) +
  scale_fill_viridis_c(option = "H", na.value = NA) +
  facet_wrap(~lyr)

bt_baseline <- bt[[names(bt) %in% range_baseline[1]:range_baseline[2]]]
bt_baseline <- mean(bt_baseline, na.rm = TRUE)
names(bt_baseline) <- paste0(range_baseline[1], "-", range_baseline[2])


ggplot() +
  geom_sf(data = map_layers$akland) +
  geom_spatraster(
    data = bt_baseline
  ) +
  scale_x_continuous(limits = map_layers$plot.boundary$x) +
  scale_y_continuous(limits = map_layers$plot.boundary$y) +
  scale_fill_viridis_c(option = "H", na.value = NA)

temp_breaks <- c(-Inf, seq(3,10,1), Inf)
viridis_palette <- "H" # viridis turbo palette


four_panel_map_data <- 
  c(bt_baseline, bt[[(dim(bt)[3]-2):dim(bt)[3]]]) |> # Combine baseline and last three surveys
  as.data.frame(
    four_panel_map_data, 
    na.rm = FALSE, 
    xy = TRUE
  ) |>
  sf::st_as_sf( # Convert to sf points
    coords = c("x", "y"),
    crs = coldpool::ebs_proj_crs
  ) |>
  stars::st_rasterize() |> # Convert to stars to make polygons
  sf::st_as_sf() |>
  tidyr::pivot_longer(
    cols = 1:4,
    names_to = "year",
    values_to = "temperature"
  ) |>
  dplyr::mutate( # Set discrete cbar levels
    temperature = cut(temperature, breaks = temp_breaks)
  ) |>
  dplyr::group_by(year, temperature) |> # Create multipolygons
  dplyr::summarise(do_union = TRUE)

n_temp_breaks <- length(temp_breaks)-1

temp_map_cbar <- 
  coldpool::legend_discrete_cbar(
    breaks = temp_breaks,
    colors = viridis::viridis_pal(option = viridis_palette)(n_temp_breaks),
    legend_direction = "vertical",
    font_size = 3,
    width = 0.1,
    expand_size.x = 0.3,
    expand_size.y = 0.3,
    expand.x = 0.3,
    expand.y = 0.9,
    spacing_scaling = 1,
    text.hjust = 0.2,
    font.family = "sans",
    neat.labels = FALSE
  ) + 
  annotate(
    "text", 
    x = 1.15, 
    y = 10, 
    label =  expression(bold("BT"~(degree*C))), 
    size = rel(3.2)
  ) + 
  theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))


plot_four_panel_map <- 
  ggplot() +
  geom_sf(
    data = four_panel_map_data,
    mapping = aes(fill = temperature), color = NA
  ) +
  geom_sf(data = map_layers$akland, color = NA, fill = "grey40", linewidth = rel(0.2)) +
  geom_sf(data = map_layers$graticule, alpha = 0.3, linewidth = rel(0.2)) +
  scale_x_continuous(limits = map_layers$plot.boundary$x) +
  scale_y_continuous(limits = map_layers$plot.boundary$y) +
  ggplot2::scale_fill_manual(values = viridis_pal(option = viridis_palette)(n_temp_breaks),
                             drop = FALSE) +
  facet_wrap(~year, nrow = 4) +
  coldpool::theme_multi_map_blue_strip() +
  theme(legend.position = "none")


ragg::agg_png(
  filename = here::here("plots", region, paste0(sel_year, "_", region, "_bottom_temperature_map.png")), 
  width = 5, 
  height = 6, 
  units = "in", 
  res = fig_res
)
print(
  cowplot::plot_grid(
    plot_four_panel_map,
    temp_map_cbar,
    ncol = 2,
    rel_widths = c(0.8,0.2)
  )
)
dev.off()
