# Explore plotting options
library(coldpool)
library(tidyterra)
library(spmodel)
library(shadowtext)

fig_res <- 300

survey_definition_id <- 52

# Setup
if(all(survey_definition_id == 47)) {
  utmcrs <- "EPSG:32605"
  region <- "GOA"
  min_year <- 1993
  sel_year <- 2025
  range_baseline <- c(1993, 2013)
  bt_breaks <- c(-Inf, seq(3,10,1), Inf)
  bt_diff_breaks <- c(-Inf, -3:3, Inf)
  contrast_years <- c(1999, 2019)
  subarea_levels <- c("Western Gulf of Alaska", "Eastern Gulf of Alaska") # Panel/timeseries order
  
  esr_ecoregion_labels <- 
    data.frame(
      AREA_NAME = c("Eastern Gulf of Alaska", "Western Gulf of Alaska"),
      AREA_ABBV = c("EGOA", "WGOA"),
      x = c(817475.92, -89219.09),
      y = c(871701.8, 490000)
    )
}

if(all(survey_definition_id == 52)) {
  utmcrs <- "EPSG:32660"
  region <- "AI"
  min_year <- 1994
  sel_year <- 2024
  range_baseline <- c(1994, 2012)
  bt_breaks <- c(-Inf, seq(3.5,6,0.5), Inf)
  bt_diff_breaks <- c(-Inf, seq(-1.5,1.5, 0.5), Inf)
  contrast_years <- c(2010, 2016)
  subarea_levels <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians") # Panel/timeseries order
  
  esr_ecoregion_labels <- 
    data.frame(
      AREA_NAME = c("Western Aleutians", "Central Aleutians", "Eastern Aleutians"),
      AREA_ABBV = c("WAI", "CAI", "EAI"),
      x = c(-2175000, -1545691.5, -901946.6),
      y = c(700000, 550000, 410000)
    )
}

dir.create(here::here("plots", region), recursive = TRUE, showWarnings = FALSE)

bt <- readRDS(here::here("output", paste0(region, "_bt.rds")))

map_layers <- 
  akgfmaps::get_base_layers(
    select.region = region, 
    set.crs = coldpool::ebs_proj_crs
  )

esr_ecoregions <- 
  akgfmaps::get_esr_regions(set.crs = 3338) |>
  dplyr::inner_join(
    data.frame(
      AREA_NAME = c("Central Aleutians", "Western Aleutians", "Eastern Aleutians", "Western Gulf of Alaska", "Eastern Gulf of Alaska"),
      AREA_ABBV = c("CAI", "WAI", "EAI", "WGOA", "EGOA"),
      by = "AREA_NAME"
    )
  ) |>
  dplyr::filter(
    AREA_NAME %in% subarea_levels
  )


# Load bathymetry raster, mask to survey extent, trim whitespace, convert to sf, change depth column name to match model
bathy <- 
  system.file("extdata", "bathymetry.gpkg", package = "akgfmaps") |>
  terra::rast() |>
  terra::mask(map_layers$survey.area) |>
  terra::trim() 

# Load AKFIN haul data to check layers
haul_data <- 
  readRDS(here::here("data", region, paste0(region, "_akfin_haul.rds"))) |>
  sf::st_as_sf(coords = c("LONGITUDE_DD_START", "LATITUDE_DD_START"), crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338")

# Bottom temperature anomaly maps ------------------------------------------------------------------
# Calculate cell-wise statistics 
# Mean, standard deviation, Z-score anomaly relative to historical baseline and all years

bt_baseline <- bt[[names(bt) %in% range_baseline[1]:range_baseline[2]]]

bt_baseline_mean <- mean(bt_baseline, na.rm = TRUE)
names(bt_baseline_mean) <- paste0(range_baseline[1], "-", range_baseline[2])

bt_baseline_sd <- stdev(bt_baseline, na.rm = TRUE)
bt_anomaly_to_baseline <- c(bt-bt_baseline_mean)/bt_baseline_sd

bt_anomaly_to_baseline <- 
  classify(
    bt_anomaly_to_baseline, 
    rcl = c(-Inf, -2, -1, 1, 2, Inf)
  )

bt_anomaly_full_ts <- c(bt-mean(bt, na.rm = TRUE))/stdev(bt, na.rm = TRUE)

bt_anomaly_full_ts <- 
  classify(
    bt_anomaly_full_ts, 
    rcl = c(-Inf, -2, -1, 1, 2, Inf)
  )

# Anomaly maps

zscore_breaks <- c(-Inf, -2, -1, 1, 2, Inf)
n_zscore_breaks <- length(zscore_breaks)-1
zscore_colors <- c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C")

zscore_cbar <- 
  coldpool::legend_discrete_cbar(
    breaks = zscore_breaks,
    colors = zscore_colors,
    legend_direction = "horizontal",
    font_size = 4.5,
    width = 0.1,
    expand_size.x = 0.3,
    expand_size.y = 0.3,
    expand.x = 0.3,
    expand.y = 0.9,
    spacing_scaling = 2,
    text.hjust = 0.5,
    font.family = "sans",
    neat.labels = FALSE
  ) + 
  annotate(
    "text", 
    x = 1.3, 
    y = 0, 
    label = "BT anomaly (Z-score)", 
    size = 4
  ) + 
  theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))

p_anomaly_to_baseline <- 
  ggplot() +
  geom_spatraster(data = bt_anomaly_to_baseline) +
  scale_fill_manual(
    name = "Anomaly (Z-score)",
    values = zscore_colors,
    labels = c("<-2", "-2–-1", "-1–1", "1–2", ">2"),
    drop = TRUE,
    na.translate = FALSE
  ) +
  geom_sf(data = map_layers$akland, color = NA, fill = "grey40", linewidth = rel(0.2)) +
  geom_sf(data = map_layers$graticule, alpha = 0.3, linewidth = rel(0.2)) +
  scale_x_continuous(limits = map_layers$plot.boundary$x) +
  scale_y_continuous(limits = map_layers$plot.boundary$y) +
  facet_wrap(~lyr, ncol = 3) +
  coldpool::theme_multi_map_blue_strip() +
  theme()

png(
  filename = here::here("plots", region, paste0(sel_year, "_", region, "_bt_anomaly_rel_baseline.png")),
  width = 7,
  height = ceiling(dim(bt_anomaly_to_baseline)[3]/3)+1,
  units = "in",
  res = fig_res
)
print(
  cowplot::plot_grid(
    p_anomaly_to_baseline + 
      theme(
        legend.position = "none",
        axis.text = element_text(size = 7.5),
        plot.margin = unit(c(5,5,-5,5), units = "pt")
      ),
    zscore_cbar + 
      theme(plot.margin = unit(c(-5,5,0,5), units = "pt")),
    rel_heights = c(0.85, 0.15),
    nrow = 2
  )
)
dev.off()

p_anomaly_full_ts <-
  ggplot() +
  geom_spatraster(data = bt_anomaly_full_ts) +
  scale_fill_manual(
    values = zscore_colors,
    labels = c("<-2", "-2–-1", "-1–1", "1–2", ">2"),
    drop = TRUE,
    na.translate = FALSE
  ) +
  geom_sf(data = map_layers$akland, color = NA, fill = "grey40", linewidth = rel(0.2)) +
  geom_sf(data = map_layers$graticule, alpha = 0.3, linewidth = rel(0.2)) +
  scale_x_continuous(limits = map_layers$plot.boundary$x) +
  scale_y_continuous(limits = map_layers$plot.boundary$y) +
  facet_wrap(~lyr, ncol = 3) +
  coldpool::theme_multi_map_blue_strip()

png(
  filename = here::here("plots", region, paste0(sel_year, "_", region, "_bt_anomaly_full.png")),
  width = 7,
  height = ceiling(dim(bt_anomaly_to_baseline)[3]/3)+1,
  units = "in",
  res = fig_res
)
print(
  cowplot::plot_grid(
    p_anomaly_full_ts + 
      theme(
        legend.position = "none",
        axis.text = element_text(size = 7.5),
        plot.margin = unit(c(5,5,-5,5), units = "pt")
      ),
    zscore_cbar + 
      theme(plot.margin = unit(c(-5,5,0,5), units = "pt")),
    rel_heights = c(0.85, 0.15),
    nrow = 2
  )
)
dev.off()


# Bottom temperature maps all years ----------------------------------------------------------------
viridis_palette <- "H" # viridis turbo palette
n_bt_breaks <- length(bt_breaks)-1

bt_map_cbar <- 
  coldpool::legend_discrete_cbar(
    breaks = bt_breaks,
    colors = viridis::viridis_pal(option = viridis_palette)(n_bt_breaks),
    legend_direction = "vertical",
    font_size = 4,
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
    y = max(bt_breaks[!is.infinite(bt_breaks)]), 
    label = "BT (\u00B0C)", 
    size = 4
  ) + 
  theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))

bt_factored <- 
  bt |>
  as.data.frame(
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
    cols = 1:dim(bt)[3],
    names_to = "year",
    values_to = "temperature"
  ) |>
  dplyr::mutate( # Set discrete cbar levels
    temperature = cut(temperature, breaks = bt_breaks)
  ) |>
  dplyr::group_by(year, temperature) |> # Create multipolygons
  dplyr::summarise(do_union = TRUE)
  
p_temp_all_years <- 
  ggplot() +
  geom_sf(
    data = bt_factored,
    mapping = aes(fill = temperature), 
    color = NA
  ) +
  geom_sf(data = map_layers$akland, color = NA, fill = "grey40", linewidth = rel(0.2)) +
  geom_sf(data = map_layers$graticule, alpha = 0.3, linewidth = rel(0.2)) +
  scale_x_continuous(limits = map_layers$plot.boundary$x,
                     breaks = map_layers$lon.breaks) +
  scale_y_continuous(limits = map_layers$plot.boundary$y,
                     breaks = map_layers$lat.breaks) +
  ggplot2::scale_fill_manual(values = viridis_pal(option = viridis_palette)(n_bt_breaks),
                             drop = FALSE,
                             na.translate = FALSE) +
  facet_wrap(~year, ncol = 2) +
  coldpool::theme_multi_map_blue_strip() +
  theme(legend.position = "none")

ragg::agg_png(filename = here::here("plots", region, paste0(region, "_bt_annual_maps.png")),
              width = 6, height = 8,
              units = "in", res = fig_res)
print(
  cowplot::plot_grid(
    p_temp_all_years,
    bt_map_cbar,
    rel_widths = c(0.85, 0.15),
    ncol = 2
  )
)
dev.off()


# Four panel bottom temperature maps ---------------------------------------------------------------
four_panel_map_data <- 
  c(bt_baseline_mean, bt[[(dim(bt)[3]-2):dim(bt)[3]]]) |> # Combine baseline and last three surveys
  as.data.frame(
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
    temperature = cut(temperature, breaks = bt_breaks)
  ) |>
  dplyr::group_by(year, temperature) |> # Create multipolygons
  dplyr::summarise(do_union = TRUE)

plot_four_panel_map <- 
  ggplot() +
  geom_sf(
    data = four_panel_map_data,
    mapping = aes(fill = temperature), color = NA
  ) +
  geom_sf(data = map_layers$akland, color = NA, fill = "grey40", linewidth = rel(0.2)) +
  geom_sf(data = map_layers$graticule, alpha = 0.3, linewidth = rel(0.2)) +
  scale_x_continuous(limits = map_layers$plot.boundary$x,
                     breaks = map_layers$lon.breaks) +
  scale_y_continuous(limits = map_layers$plot.boundary$y,
                     breaks = map_layers$lat.breaks) +
  ggplot2::scale_fill_manual(values = viridis_pal(option = viridis_palette)(n_bt_breaks),
                             drop = FALSE) +
  facet_wrap(~year, ncol = 1) +
  coldpool::theme_multi_map_blue_strip() +
  theme(legend.position = "none")


ragg::agg_png(
  filename = here::here("plots", region, paste0(sel_year, "_", region, "_bottom_temperature_map.png")), 
  width = 6.5, 
  height = 7.2, 
  units = "in", 
  res = fig_res
)
print(
  cowplot::plot_grid(
    plot_four_panel_map + theme(strip.text = element_text(size = 11)),
    bt_map_cbar,
    ncol = 2,
    rel_widths = c(0.8,0.2)
  )
)
dev.off()

## Contrast BT years -------------------------------------------------------------------------------

bt_diff_pal <- "RdBu"
n_bt_diff_breaks <- length(bt_diff_breaks)-1

bt_diff_cbar <- 
  coldpool::legend_discrete_cbar(
    breaks = bt_diff_breaks,
    colors = scales::brewer_pal(palette = bt_diff_pal, direction = -1)(n_bt_diff_breaks),
    legend_direction = "vertical",
    font_size = 4,
    width = 0.1,
    expand_size.x = 0.3,
    expand_size.y = 0.2,
    expand.x = 0.3,
    expand.y = 1.4,
    spacing_scaling = 1,
    text.hjust = 0.2,
    font.family = "sans",
    neat.labels = TRUE
  ) + 
  annotate(
    "text", 
    x = 1.15, 
    y = max(bt_diff_breaks[!is.infinite(bt_diff_breaks)]) + ifelse(region == "GOA", 3, 2), 
    label = "\u0394BT (\u00B0C)", 
    size = 4
  ) + 
  theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))

bt_all_mean <- mean(bt, na.rm = TRUE)

names(bt_all_mean) <- paste0("Mean (", min(names(bt)), "\u2013", max(names(bt)), ")")

bt_contrast <- bt[[names(bt) %in% contrast_years]]

bt_contrast <-
  c(bt_all_mean, bt_contrast) |>
  as.data.frame(
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
    cols = 1:3,
    names_to = "year",
    values_to = "temperature"
  ) |>
  dplyr::mutate( # Set discrete cbar levels
    temperature = cut(temperature, breaks = bt_breaks)
  ) |>
  dplyr::group_by(year, temperature) |> # Create multipolygons
  dplyr::summarise(do_union = TRUE)


plot_contrast_years <-
  ggplot() +
  geom_sf(
    data = esr_ecoregions,
    mapping = aes(color = AREA_ABBV),
    fill = NA,
    linewidth = 0.4
  ) +
  geom_sf(
    data = bt_contrast,
    mapping = aes(fill = temperature),
    color = NA
  ) +
  geom_sf(
    data = map_layers$akland,
    color = NA,
    fill = "grey70",
    linewidth = rel(0.2)
    ) +
  geom_shadowtext(
    data = esr_ecoregion_labels |>
      dplyr::mutate(year = paste0("Mean (", min(names(bt)), "\u2013", max(names(bt)), ")")),
    mapping = aes(x = x, y = y, color = AREA_ABBV, label = AREA_ABBV),
    bg.color = "white",
    size = 4
  ) +
  scale_color_manual(
    values = c(
      "WAI" = "#8FD744FF",
      "CAI" = "#35B779FF",
      "EAI" = "#21908CFF",
      "WGOA" = "#31688EFF",
      "EGOA" = "#440154FF"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = map_layers$plot.boundary$x,
    breaks = map_layers$lon.breaks
  ) +
  scale_y_continuous(
    limits = map_layers$plot.boundary$y,
    breaks = map_layers$lat.breaks
  ) +
  ggplot2::scale_fill_manual(
    values = viridis_pal(option = viridis_palette)(n_bt_breaks),
                             drop = FALSE
    ) +
  facet_wrap(~factor(
    year,
    levels =
      c(paste0("Mean (", min(names(bt)), "\u2013", max(names(bt)), ")"), contrast_years, paste0("BT[" , contrast_years[2], "] - BT[", contrast_years[1], "]"))
  ),
  ncol = 1) +
  coldpool::theme_multi_map_blue_strip() +
  theme(
    legend.position = "none",
        panel.grid.major = element_line(linewidth = 0.2, color = "grey92"),
    axis.title = element_blank()
    )

bt_diff <- bt[[as.character(contrast_years[2])]] - bt[[as.character(contrast_years[1])]]

names(bt_diff) <- paste0("BT[" , contrast_years[2], "] - BT[", contrast_years[1], "]")

bt_diff <-
  bt_diff |>
  as.data.frame(
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
    cols = 1,
    names_to = "year",
    values_to = "temperature"
  ) |>
  dplyr::mutate( # Set discrete cbar levels
    temperature = cut(temperature, breaks = bt_diff_breaks)
  ) |>
  dplyr::group_by(year, temperature) |> # Create multipolygons
  dplyr::summarise(do_union = TRUE)

plot_diff_years <-
  ggplot() +
  geom_sf(
    data = esr_ecoregions,
    mapping = aes(color = AREA_ABBV),
    fill = NA,
    linewidth = 0.4
  ) +
  geom_sf(
    data = bt_diff,
    mapping = aes(fill = temperature),
    color = NA
  ) +
  geom_sf(
    data = map_layers$akland,
    color = NA,
    fill = "grey70",
    linewidth = rel(0.2)
  ) +
  scale_color_manual(
    values = c(
      "WAI" = "#8FD744FF",
      "CAI" = "#35B779FF",
      "EAI" = "#21908CFF",
      "WGOA" = "#31688EFF",
      "EGOA" = "#440154FF"
    ),
    guide = "none"
  ) +
  scale_x_continuous(
    limits = map_layers$plot.boundary$x,
    breaks = map_layers$lon.breaks
  ) +
  scale_y_continuous(
    limits = map_layers$plot.boundary$y,
    breaks = map_layers$lat.breaks
  ) +
  ggplot2::scale_fill_manual(
    values = scales::brewer_pal(palette = bt_diff_pal, direction = -1)(n_bt_diff_breaks),
    drop = FALSE,
    na.translate = FALSE
  ) +
  facet_wrap(~year
  ) +
  coldpool::theme_multi_map_blue_strip() +
  theme(
    legend.position = "none",
    panel.grid.major = element_line(linewidth = 0.2, color = "grey92"),
    axis.title = element_blank()
  )

ragg::agg_png(
  filename = here::here("plots", region, paste0(region, "_bt_contrast_map.png")), 
  width = 6, 
  height = 6.5, 
  units = "in", 
  res = fig_res
)
print(
  cowplot::plot_grid(
    cowplot::plot_grid(
      plot_contrast_years + 
        theme(
          plot.margin = unit(c(5,-5,5, 5), units = "pt"),
          strip.text = element_text(size = 11), 
          axis.ticks.x = element_blank(), 
          axis.text.x = element_blank()
          ),
      plot_diff_years + 
        theme(strip.text = element_text(size = 11),
              plot.margin = unit(c(0,-5,5, 5), units = "pt")),
      align = "hv",
      # axis = "tblr",
      nrow = 2,
      rel_heights = c(0.7, 0.265)
    ),
    cowplot::plot_grid(
      bt_map_cbar +
        theme(plot.margin = unit(c(2, 2 ,2, 0), units = "pt")),
      bt_diff_cbar +
        theme(plot.margin = unit(c(2, 2 ,2, 0), units = "pt")),
      nrow = 2,
      rel_heights = c(0.7, 0.265)
    ),
    rel_widths = c(0.8, 0.15),
    ncol = 2
  )
)
dev.off()


## Surface temperatures ----------------------------------------------------------------------------

sst <- readRDS(here::here("output", paste0(region, "_sst.rds")))

# Calculate cell-wise statistics 
# Mean, standard deviation, Z-score anomaly relative to historical baseline and all years

sst_baseline <- sst[[names(sst) %in% range_baseline[1]:range_baseline[2]]]

sst_baseline_mean <- mean(sst_baseline, na.rm = TRUE)
names(sst_baseline_mean) <- paste0(range_baseline[1], "-", range_baseline[2])

sst_baseline_sd <- stdev(sst_baseline, na.rm = TRUE)
sst_anomaly_to_baseline <- c(sst-sst_baseline_mean)/sst_baseline_sd

sst_anomaly_to_baseline <- 
  classify(
    sst_anomaly_to_baseline, 
    rcl = c(-Inf, -2, -1, 1, 2, Inf)
  )

sst_anomaly_full_ts <- c(sst-mean(sst, na.rm = TRUE))/stdev(sst, na.rm = TRUE)

sst_anomaly_full_ts <- 
  classify(
    sst_anomaly_full_ts, 
    rcl = c(-Inf, -2, -1, 1, 2, Inf)
  )

# Anomaly maps

zscore_breaks <- c(-Inf, -2, -1, 1, 2, Inf)
n_zscore_breaks <- length(zscore_breaks)-1
zscore_colors <- c("#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C")

sst_zscore_cbar <-
  coldpool::legend_discrete_cbar(
    breaks = zscore_breaks,
    colors = zscore_colors,
    legend_direction = "horizontal",
    font_size = 4,
    width = 0.1,
    expand_size.x = 0.3,
    expand_size.y = 0.3,
    expand.x = 0.3,
    expand.y = 0.9,
    spacing_scaling = 2,
    text.hjust = 0.5,
    font.family = "sans",
    neat.labels = FALSE
  ) +
  annotate(
    "text",
    x = 1.3,
    y = 0,
    label = "SST anomaly (Z-score)",
    size = 4
  ) +
  theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))

p_anomaly_to_baseline_sst <- 
  ggplot() +
  geom_spatraster(data = sst_anomaly_to_baseline) +
  scale_fill_manual(
    name = "Anomaly (Z-score)",
    values = zscore_colors,
    labels = c("<-2", "-2–-1", "-1–1", "1–2", ">2"),
    drop = TRUE,
    na.translate = FALSE
  ) +
  geom_sf(data = map_layers$akland, color = NA, fill = "grey40", linewidth = rel(0.2)) +
  geom_sf(data = map_layers$graticule, alpha = 0.3, linewidth = rel(0.2)) +
  scale_x_continuous(limits = map_layers$plot.boundary$x) +
  scale_y_continuous(limits = map_layers$plot.boundary$y) +
  facet_wrap(~lyr, ncol = 3) +
  coldpool::theme_multi_map_blue_strip() +
  theme()

png(
  filename = here::here("plots", region, paste0(sel_year, "_", region, "_sst_anomaly_rel_baseline.png")),
  width = 7,
  height = ceiling(dim(sst_anomaly_to_baseline)[3]/3)+1,
  units = "in",
  res = fig_res
)
print(
  cowplot::plot_grid(
    p_anomaly_to_baseline_sst + 
      theme(
        legend.position = "none",
        axis.text = element_text(size = 7.5),
        plot.margin = unit(c(5,5,-5,5), units = "pt")
      ),
    sst_zscore_cbar + 
      theme(plot.margin = unit(c(-5,5,0,5), units = "pt")),
    rel_heights = c(0.85, 0.15),
    nrow = 2
  )
)
dev.off()

# png(
#   filename = here::here("plots", region, paste0(sel_year, "_", region, "_sst_anomaly_rel_baseline_title.png")),
#   width = 7,
#   height = ceiling(dim(sst_anomaly_to_baseline)[3]/3)+1,
#   units = "in",
#   res = fig_res
# )
# print(
#   cowplot::plot_grid(
#     p_anomaly_to_baseline_sst + 
#       ggtitle(paste0("Sea surface temperature anomaly (Z-score) relative to ", range_baseline[1], " to ", range_baseline[2], " mean")) +
#       theme(
#         legend.position = "none",
#         axis.text = element_text(size = 7.5),
#         plot.margin = unit(c(5,5,-5,5), units = "pt")
#       ),
#     sst_zscore_cbar + 
#       theme(plot.margin = unit(c(-5,5,0,5), units = "pt")),
#     rel_heights = c(0.85, 0.15),
#     nrow = 2
#   )
# )
# dev.off()

p_anomaly_full_ts_sst <-
  ggplot() +
  geom_spatraster(data = sst_anomaly_full_ts) +
  scale_fill_manual(
    values = zscore_colors,
    labels = c("<-2", "-2–-1", "-1–1", "1–2", ">2"),
    drop = TRUE,
    na.translate = FALSE
  ) +
  geom_sf(data = map_layers$akland, color = NA, fill = "grey40", linewidth = rel(0.2)) +
  geom_sf(data = map_layers$graticule, alpha = 0.3, linewidth = rel(0.2)) +
  scale_x_continuous(limits = map_layers$plot.boundary$x) +
  scale_y_continuous(limits = map_layers$plot.boundary$y) +
  facet_wrap(~lyr, ncol = 3) +
  coldpool::theme_multi_map_blue_strip()

# png(
#   filename = here::here("plots", region, paste0(sel_year, "_", region, "_sst_anomaly_full_title.png")),
#   width = 7,
#   height = ceiling(dim(sst_anomaly_to_baseline)[3]/3)+1,
#   units = "in",
#   res = fig_res
# )
# print(
#   cowplot::plot_grid(
#     p_anomaly_full_ts_sst + 
#       ggtitle(
#         paste0("Sea surface temperature anomaly (Z-score) relative to full timeseries")
#       ) +
#       theme(
#         legend.position = "none",
#         axis.text = element_text(size = 7.5),
#         plot.margin = unit(c(5,5,-5,5), units = "pt")
#       ),
#     sst_zscore_cbar + 
#       theme(plot.margin = unit(c(-5,5,0,5), units = "pt")),
#     rel_heights = c(0.85, 0.15),
#     nrow = 2
#   )
# )
# dev.off()

png(
  filename = here::here("plots", region, paste0(sel_year, "_", region, "_sst_anomaly_full.png")),
  width = 7,
  height = ceiling(dim(sst_anomaly_to_baseline)[3]/3)+1,
  units = "in",
  res = fig_res
)
print(
  cowplot::plot_grid(
    p_anomaly_full_ts_sst + 
      theme(
        legend.position = "none",
        axis.text = element_text(size = 7.5),
        plot.margin = unit(c(5,5,-5,5), units = "pt")
      ),
    sst_zscore_cbar + 
      theme(plot.margin = unit(c(-5,5,0,5), units = "pt")),
    rel_heights = c(0.85, 0.15),
    nrow = 2
  )
)
dev.off()



# Four panel surface temperature maps --------------------------------------------------------------
temp_breaks_sst <- c(-Inf, seq(4,14,2), Inf)
viridis_palette <- "H" # viridis turbo palette
n_temp_breaks_sst <- length(temp_breaks_sst)-1


four_panel_map_data_sst <- 
  c(sst_baseline_mean, sst[[(dim(sst)[3]-2):dim(sst)[3]]]) |> # Combine baseline and last three surveys
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
    temperature = cut(temperature, breaks = temp_breaks_sst)
  ) |>
  dplyr::group_by(year, temperature) |> # Create multipolygons
  dplyr::summarise(do_union = TRUE)


temp_map_cbar_sst <- 
  coldpool::legend_discrete_cbar(
    breaks = temp_breaks_sst,
    colors = viridis::viridis_pal(option = viridis_palette)(n_temp_breaks_sst),
    legend_direction = "vertical",
    font_size = 4,
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
    x = 1.3, 
    y = 10, 
    label =  "SST (\u00B0C)", 
    size = 4
  ) + 
  theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))


plot_four_panel_map_sst <- 
  ggplot() +
  geom_sf(
    data = four_panel_map_data_sst,
    mapping = aes(fill = temperature), color = NA
  ) +
  geom_sf(data = map_layers$akland, color = NA, fill = "grey40", linewidth = rel(0.2)) +
  geom_sf(data = map_layers$graticule, alpha = 0.3, linewidth = rel(0.2)) +
  scale_x_continuous(limits = map_layers$plot.boundary$x) +
  scale_y_continuous(limits = map_layers$plot.boundary$y) +
  ggplot2::scale_fill_manual(values = viridis_pal(option = viridis_palette)(n_temp_breaks_sst),
                             drop = FALSE) +
  facet_wrap(~year, nrow = 4) +
  coldpool::theme_multi_map_blue_strip() +
  theme(legend.position = "none")


ragg::agg_png(
  filename = here::here("plots", region, paste0(sel_year, "_", region, "_sst_map.png")), 
  width = 6.5, 
  height = 7.2, 
  units = "in", 
  res = fig_res
)
print(
  cowplot::plot_grid(
    plot_four_panel_map_sst,
    temp_map_cbar_sst,
    ncol = 2,
    rel_widths = c(0.8,0.2)
  )
)
dev.off()
