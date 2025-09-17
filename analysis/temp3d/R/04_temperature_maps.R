# Explore plotting options
library(coldpool)
library(tidyterra)
library(spmodel)

fig_res <- 300

survey_definition_id = 52

dir.create(here::here("plots", region), recursive = TRUE, showWarnings = FALSE)

# Setup
if(all(survey_definition_id == 47)) {
  utmcrs <- "EPSG:32605"
  region <- "GOA"
  min_year <- 1993
  sel_year <- 2025
  range_baseline <- c(1993, 2013)
  subarea_levels <- c("Western Gulf of Alaska", "Eastern Gulf of Alaska") # Panel/timeseries order
}

if(all(survey_definition_id == 52)) {
  utmcrs <- "EPSG:32660"
  region <- "AI"
  min_year <- 1994
  sel_year <- 2024
  range_baseline <- c(1994, 2012)
  subarea_levels <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians") # Panel/timeseries order
}

bt <- readRDS(here::here("output", paste0(region, "_bt.rds")))

map_layers <- 
  akgfmaps::get_base_layers(
    select.region = region, 
    set.crs = coldpool::ebs_proj_crs
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
    font_size = 3,
    width = 0.1,
    expand_size.x = 0.3,
    expand_size.y = 0.3,
    expand.x = 0.3,
    expand.y = 0.9,
    spacing_scaling = 1,
    text.hjust = 0.5,
    font.family = "sans",
    neat.labels = FALSE
  ) + 
  annotate(
    "text", 
    x = 1.15, 
    y = 0, 
    label = expression(bold("BT anomaly (Z-score)")), 
    size = rel(3.2)
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

png(
  filename = here::here("plots", region, paste0(sel_year, "_", region, "_bt_anomaly_rel_baseline_title.png")),
  width = 7,
  height = ceiling(dim(bt_anomaly_to_baseline)[3]/3)+1,
  units = "in",
  res = fig_res
)
print(
  cowplot::plot_grid(
    p_anomaly_to_baseline + 
      ggtitle(paste0("Bottom temperature anomaly (Z-score) relative to ", range_baseline[1], " to ", range_baseline[2], " mean")) +
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
  filename = here::here("plots", region, paste0(sel_year, "_", region, "_bt_anomaly_full_title.png")),
  width = 7,
  height = ceiling(dim(bt_anomaly_to_baseline)[3]/3)+1,
  units = "in",
  res = fig_res
)
print(
  cowplot::plot_grid(
    p_anomaly_full_ts + 
      ggtitle(
        paste0("Bottom temperature anomaly (Z-score) relative to full timeseries")
      ) +
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



# Four panel bottom temperature maps ---------------------------------------------------------------
temp_breaks <- c(-Inf, seq(3,10,1), Inf)
viridis_palette <- "H" # viridis turbo palette
n_temp_breaks <- length(temp_breaks)-1


four_panel_map_data <- 
  c(bt_baseline_mean, bt[[(dim(bt)[3]-2):dim(bt)[3]]]) |> # Combine baseline and last three surveys
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


# Subarea bottom temperature time series -----------------------------------------------------------

esr_subareas <- 
  akgfmaps::get_esr_regions(select.region = "esr_subarea", set.crs = "EPSG:3338")

subarea_bt <- data.frame()

for(ii in 1:length(subarea_levels)) {
  
  sel_subarea <- 
    dplyr::filter(
      esr_subareas,
      AREA_NAME == subarea_levels[ii]
    )
  
  # Mask to subarea, calculate mean for each year, rename gear temperature column
  subarea_bt <- 
    dplyr::bind_rows(
      subarea_bt,
      terra::mask(
        bt,
        sel_subarea,
        touches = TRUE
      ) |>
        terra::global(
          fun = "mean", 
          na.rm = TRUE) |>
        dplyr::mutate(
          YEAR = as.numeric(names(bt)),
          AREA_NAME = sel_subarea$AREA_NAME
        ) |>
        dplyr::rename(MEAN_GEAR_TEMPERATURE = mean) |>
        dplyr::filter(!is.na(MEAN_GEAR_TEMPERATURE)) # Handle 2001
    )
  
}


# Bottom temperature time series relative to baseline period ---------------------------------------
subarea_baseline <- subarea_bt |>
  dplyr::filter(YEAR >= range_baseline[1] & YEAR <= range_baseline[2]) |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::summarise(
    SD_GEAR_TEMPERATURE = sd(MEAN_GEAR_TEMPERATURE),
    MEAN_GEAR_TEMPERATURE = mean(MEAN_GEAR_TEMPERATURE)
  ) |>
  dplyr::mutate(
    MIN_YEAR = range_baseline[1],
    MAX_YEAR = range_baseline[2]
  )

ggplot() +
  geom_path(data = subarea_baseline |>
              tidyr::pivot_longer(cols = c("MIN_YEAR", "MAX_YEAR")),
            mapping = aes(x = value, y = MEAN_GEAR_TEMPERATURE),
            linetype = 1) +
  geom_path(data = subarea_baseline |>
              tidyr::pivot_longer(cols = c("MIN_YEAR", "MAX_YEAR")),
            mapping = aes(x = value, y = MEAN_GEAR_TEMPERATURE + SD_GEAR_TEMPERATURE),
            linetype = 2) +
  geom_path(data = subarea_baseline |>
              tidyr::pivot_longer(cols = c("MIN_YEAR", "MAX_YEAR")),
            mapping = aes(x = value, y = MEAN_GEAR_TEMPERATURE - SD_GEAR_TEMPERATURE),
            linetype = 2) +
  geom_point(data = subarea_bt,
             mapping = aes(x = YEAR, y = MEAN_GEAR_TEMPERATURE),
             color = "#0085CA") +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = expression('Mean bottom temperature ('*degree*C*')')) +
  facet_wrap(~factor(AREA_NAME, levels = subarea_levels)) +
  theme_timeseries_blue_strip()

z_levels <- factor(
  c(paste0("Mean (", range_baseline[1], "–", range_baseline[2], ")"), "\u00B1 1 SD"),
  levels = c(paste0("Mean (", range_baseline[1], "–", range_baseline[2], ")"), "\u00B1 1 SD")
)

year_breaks <- seq(
  plyr::round_any(c(min_year), 2, floor),
  plyr::round_any(c(sel_year), 2, floor),
  by = 2
)

year_lab <- year_breaks

year_lab[!(year_lab %in% seq(
  plyr::round_any(c(min_year), 2, floor),
  plyr::round_any(c(sel_year), 4, floor),
  by = 4
))] <- ""
  

p_bt_timeseries <- 
  ggplot() +
  geom_hline(data = subarea_baseline,
             mapping = aes(
               yintercept = MEAN_GEAR_TEMPERATURE, 
               linetype = z_levels[1]
             ),
             color = "grey50") +
  geom_hline(data = subarea_baseline,
             mapping = aes(
               yintercept = MEAN_GEAR_TEMPERATURE + SD_GEAR_TEMPERATURE, 
               linetype = z_levels[2]),
             color = "grey50") +
  geom_hline(data = subarea_baseline,
             mapping = aes(yintercept = MEAN_GEAR_TEMPERATURE - SD_GEAR_TEMPERATURE, 
                           linetype = z_levels[2]),
             color = "grey50") +
  geom_point(data = subarea_bt,
             mapping = aes(x = YEAR, y = MEAN_GEAR_TEMPERATURE),
             color = "#0085CA") +
  scale_x_continuous(name = "Year", breaks = year_breaks, labels = year_lab) +
  scale_y_continuous(name = expression('Mean bottom temperature ('*degree*C*')')) +
  facet_wrap(~factor(AREA_NAME, levels = subarea_levels)) +
  theme_timeseries_blue_strip() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank())

png(here::here("plots", region, paste0(sel_year, "_", region, "_bt_timeseries.png")),
    width = 7, height = 3, units = "in", res = 300)
print(p_bt_timeseries)
dev.off()


# Bottom temperature time series relative to full time series --------------------------------------

subarea_baseline <- subarea_bt |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::summarise(
    SD_GEAR_TEMPERATURE = sd(MEAN_GEAR_TEMPERATURE),
    MEAN_GEAR_TEMPERATURE = mean(MEAN_GEAR_TEMPERATURE)
  )

z_levels <- factor(
  c("Mean", "\u00B1 1 SD"),
  levels = c("Mean", "\u00B1 1 SD")
)

year_breaks <- seq(
  plyr::round_any(c(min_year), 2, floor),
  plyr::round_any(c(sel_year), 2, floor),
  by = 2
)

year_lab <- year_breaks

year_lab[!(year_lab %in% seq(
  plyr::round_any(c(min_year), 2, floor),
  plyr::round_any(c(sel_year), 4, floor),
  by = 4
))] <- ""


p_bt_timeseries_no_baseline <- 
  ggplot() +
  geom_hline(data = subarea_baseline,
             mapping = aes(
               yintercept = MEAN_GEAR_TEMPERATURE, 
               linetype = z_levels[1]
             ),
             color = "grey50") +
  geom_hline(data = subarea_baseline,
             mapping = aes(
               yintercept = MEAN_GEAR_TEMPERATURE + SD_GEAR_TEMPERATURE, 
               linetype = z_levels[2]),
             color = "grey50") +
  geom_hline(data = subarea_baseline,
             mapping = aes(yintercept = MEAN_GEAR_TEMPERATURE - SD_GEAR_TEMPERATURE, 
                           linetype = z_levels[2]),
             color = "grey50") +
  geom_point(data = subarea_bt,
             mapping = aes(x = YEAR, y = MEAN_GEAR_TEMPERATURE),
             color = "#0085CA") +
  scale_x_continuous(name = "Year", breaks = year_breaks, labels = year_lab) +
  scale_y_continuous(name = expression('Mean bottom temperature ('*degree*C*')')) +
  facet_wrap(~factor(AREA_NAME, levels = subarea_levels)) +
  theme_timeseries_blue_strip() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank())

png(here::here("plots", region, paste0(sel_year, "_", region, "_bt_timeseries_no_baseline.png")),
    width = 7, height = 3, units = "in", res = 300)
print(p_bt_timeseries_no_baseline)
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
    font_size = 3,
    width = 0.1,
    expand_size.x = 0.3,
    expand_size.y = 0.3,
    expand.x = 0.3,
    expand.y = 0.9,
    spacing_scaling = 1,
    text.hjust = 0.5,
    font.family = "sans",
    neat.labels = FALSE
  ) +
  annotate(
    "text",
    x = 1.15,
    y = 0,
    label = expression(bold("SST anomaly (Z-score)")),
    size = rel(3.2)
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

png(
  filename = here::here("plots", region, paste0(sel_year, "_", region, "_sst_anomaly_rel_baseline_title.png")),
  width = 7,
  height = ceiling(dim(sst_anomaly_to_baseline)[3]/3)+1,
  units = "in",
  res = fig_res
)
print(
  cowplot::plot_grid(
    p_anomaly_to_baseline_sst + 
      ggtitle(paste0("Sea surface temperature anomaly (Z-score) relative to ", range_baseline[1], " to ", range_baseline[2], " mean")) +
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

png(
  filename = here::here("plots", region, paste0(sel_year, "_", region, "_sst_anomaly_full_title.png")),
  width = 7,
  height = ceiling(dim(sst_anomaly_to_baseline)[3]/3)+1,
  units = "in",
  res = fig_res
)
print(
  cowplot::plot_grid(
    p_anomaly_full_ts_sst + 
      ggtitle(
        paste0("Sea surface temperature anomaly (Z-score) relative to full timeseries")
      ) +
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
  width = 5, 
  height = 6, 
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



