# Explore plotting options
library(coldpool)
library(tidyterra)
library(shadowtext)


fig_res <- 300

survey_definition_id <- 52

# Setup
if(all(survey_definition_id == 47)) {
  utmcrs <- "EPSG:32605"
  region <- "GOA"
  min_year <- 1993
  max_year <- 2025 # Change for the current year
  range_baseline <- c(1993, 2013)
  subarea_levels <- c("Western Gulf of Alaska", "Eastern Gulf of Alaska") # Panel/timeseries order
  region_name <- "Gulf of Alaska"
  
  bt_breaks <- c(-Inf, seq(3,10,1), Inf)
  bt_diff_breaks <- c(-Inf, -3:3, Inf)
  contrast_years <- c(1999, 2019)
  
  esr_ecoregion_labels <- 
    data.frame(
      AREA_NAME = c("Eastern Gulf of Alaska", "Western Gulf of Alaska"),
      AREA_ABBV = c("EGOA", "WGOA"),
      x = c(817475.92, -89219.09),
      y = c(871701.8, 490000)
    )
  
  bt <- terra::unwrap(coldpool::goa_bottom_temperature)
  sst <- terra::unwrap(coldpool::goa_surface_temperature)
}

if(all(survey_definition_id == 52)) {
  utmcrs <- "EPSG:32660"
  region <- "AI"
  min_year <- 1994
  max_year <- 2026 # Change for the current year
  range_baseline <- c(1994, 2012)
  subarea_levels <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians") # Panel/timeseries order
  region_name <- "Aleutian Islands"
  
  bt_breaks <- c(-Inf, seq(3.5,6,0.5), Inf)
  bt_diff_breaks <- c(-Inf, seq(-1.5,1.5, 0.5), Inf)
  contrast_years <- c(2010, 2016)
  
  esr_ecoregion_labels <- 
    data.frame(
      AREA_NAME = c("Western Aleutians", "Central Aleutians", "Eastern Aleutians"),
      AREA_ABBV = c("WAI", "CAI", "EAI"),
      x = c(-2175000, -1545691.5, -901946.6),
      y = c(700000, 550000, 410000)
    )
  
  bt <- terra::unwrap(coldpool::ai_bottom_temperature)
  sst <- terra::unwrap(coldpool::ai_surface_temperature)
}

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

# Subarea bottom temperature time series -----------------------------------------------------------

esr_subareas <- 
  akgfmaps::get_esr_regions(select.region = "esr_subarea", set.crs = "EPSG:3338")

subarea_sst <- data.frame()
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
        dplyr::filter(!is.na(MEAN_GEAR_TEMPERATURE)) # Handle 2001 GOA
    )
  
  # Mask to subarea, calculate mean for each year, rename gear temperature column
  subarea_sst <- 
    dplyr::bind_rows(
      subarea_sst,
      terra::mask(
        sst,
        sel_subarea,
        touches = TRUE
      ) |>
        terra::global(
          fun = "mean", 
          na.rm = TRUE) |>
        dplyr::mutate(
          YEAR = as.numeric(names(sst)),
          AREA_NAME = sel_subarea$AREA_NAME
        ) |>
        dplyr::rename(MEAN_SURFACE_TEMPERATURE = mean) |>
        dplyr::filter(!is.na(MEAN_SURFACE_TEMPERATURE)) # Handle 2001 GOA
    )
  
}

region_sst <- 
  terra::global(sst, fun = "mean", na.rm = TRUE) |>
  dplyr::mutate(YEAR = as.numeric(names(sst)),
                AREA_NAME = region_name) |>
  dplyr::rename(MEAN_SURFACE_TEMPERATURE = mean) |>
  dplyr::filter(!is.na(MEAN_SURFACE_TEMPERATURE))

region_bt <-
  terra::global(bt, fun = "mean", na.rm = TRUE) |>
  dplyr::mutate(YEAR = as.numeric(names(bt)),
                AREA_NAME = region_name) |>
  dplyr::rename(MEAN_GEAR_TEMPERATURE = mean) |>
  dplyr::filter(!is.na(MEAN_GEAR_TEMPERATURE))

temperature_time_series <- 
  dplyr::full_join(region_sst, region_bt) |>
  dplyr::bind_rows(dplyr::full_join(subarea_bt, subarea_sst)) |>
  dplyr::select(YEAR, AREA_NAME, MEAN_GEAR_TEMPERATURE, MEAN_SURFACE_TEMPERATURE)

temperature_time_series <- temperature_time_series |>
  dplyr::mutate(MEAN_GEAR_TEMPERATURE = round(MEAN_GEAR_TEMPERATURE, 2),
                MEAN_SURFACE_TEMPERATURE = round(MEAN_SURFACE_TEMPERATURE, 2)) |>
  dplyr::arrange(factor(AREA_NAME, levels = c(subarea_levels, region_name)), YEAR) |>
  dplyr::mutate(LAST_UPDATE = Sys.Date())

if(all(survey_definition_id == 47)) {
  
  goa_mean_temperature <- temperature_time_series

  usethis::use_data(goa_mean_temperature, overwrite = TRUE)
  
}

if(all(survey_definition_id == 52)) {
  
  ai_mean_temperature <- temperature_time_series
  
  usethis::use_data(ai_mean_temperature, overwrite = TRUE)
  
}


# Bottom temperature time series relative to baseline period ---------------------------------------
subarea_baseline_bt <- subarea_bt |>
  dplyr::filter(YEAR >= range_baseline[1] & YEAR <= range_baseline[2]) |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::summarise(
    SD_GEAR_TEMPERATURE = round(sd(MEAN_GEAR_TEMPERATURE, na.rm = TRUE), 2),
    MAX_GEAR_TEMPERATURE = round(max(MEAN_GEAR_TEMPERATURE, na.rm = TRUE), 2),
    MIN_GEAR_TEMPERATURE = round(min(MEAN_GEAR_TEMPERATURE, na.rm = TRUE), 2),
    MEAN_GEAR_TEMPERATURE = round(mean(MEAN_GEAR_TEMPERATURE, na.rm = TRUE), 2)
  ) |>
  dplyr::mutate(
    MIN_YEAR = range_baseline[1],
    MAX_YEAR = range_baseline[2],
    PERIOD = "Baseline"
  )

subarea_recent_bt <- subarea_bt |>
  dplyr::filter(YEAR > range_baseline[2]) |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::summarise(
    SD_GEAR_TEMPERATURE = round(sd(MEAN_GEAR_TEMPERATURE, na.rm = TRUE), 2),
    MAX_GEAR_TEMPERATURE = round(max(MEAN_GEAR_TEMPERATURE, na.rm = TRUE), 2),
    MIN_GEAR_TEMPERATURE = round(min(MEAN_GEAR_TEMPERATURE, na.rm = TRUE), 2),
    MEAN_GEAR_TEMPERATURE = round(mean(MEAN_GEAR_TEMPERATURE, na.rm = TRUE), 2)
  ) |>
  dplyr::mutate(
    MIN_YEAR = min(subarea_bt$YEAR[subarea_bt$YEAR > range_baseline[2]]),
    MAX_YEAR = max(subarea_bt$YEAR),
    PERIOD = "Recent"
  )

subarea_full_bt <- subarea_bt |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::summarise(
    SD_GEAR_TEMPERATURE = round(sd(MEAN_GEAR_TEMPERATURE, na.rm = TRUE), 2),
    MAX_GEAR_TEMPERATURE = round(max(MEAN_GEAR_TEMPERATURE, na.rm = TRUE), 2),
    MIN_GEAR_TEMPERATURE = round(min(MEAN_GEAR_TEMPERATURE, na.rm = TRUE), 2),
    MEAN_GEAR_TEMPERATURE = round(mean(MEAN_GEAR_TEMPERATURE, na.rm = TRUE), 2)
  ) |>
  dplyr::mutate(
    MIN_YEAR = range_baseline[1],
    MAX_YEAR = max(subarea_bt$YEAR),
    PERIOD = "Full"
  )

subarea_baseline_sst <- subarea_sst |>
  dplyr::filter(YEAR >= range_baseline[1] & YEAR <= range_baseline[2]) |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::summarise(
    SD_SURFACE_TEMPERATURE = round(sd(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE), 2),
    MAX_SURFACE_TEMPERATURE = round(max(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE), 2),
    MIN_SURFACE_TEMPERATURE = round(min(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE), 2),
    MEAN_SURFACE_TEMPERATURE = round(mean(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE), 2)
  ) |>
  dplyr::mutate(
    MIN_YEAR = range_baseline[1],
    MAX_YEAR = range_baseline[2],
    PERIOD = "Baseline"
  )

subarea_recent_sst <- subarea_sst |>
  dplyr::filter(YEAR > range_baseline[2]) |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::summarise(
    SD_SURFACE_TEMPERATURE = round(sd(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE), 2),
    MAX_SURFACE_TEMPERATURE = round(max(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE), 2),
    MIN_SURFACE_TEMPERATURE = round(min(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE), 2),
    MEAN_SURFACE_TEMPERATURE = round(mean(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE), 2)
  ) |>
  dplyr::mutate(
    MIN_YEAR = min(subarea_bt$YEAR[subarea_bt$YEAR > range_baseline[2]]),
    MAX_YEAR = max(subarea_bt$YEAR),
    PERIOD = "Recent"
  )

subarea_full_sst <- subarea_sst |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::summarise(
    SD_SURFACE_TEMPERATURE = round(sd(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE), 2),
    MAX_SURFACE_TEMPERATURE = round(max(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE), 2),
    MIN_SURFACE_TEMPERATURE = round(min(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE), 2),
    MEAN_SURFACE_TEMPERATURE = round(mean(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE), 2)
  ) |>
  dplyr::mutate(
    MIN_YEAR = range_baseline[1],
    MAX_YEAR = max(subarea_bt$YEAR),
    PERIOD = "Full"
  )

mean_temperature_by_period <-
  dplyr::bind_rows(
    subarea_baseline_bt,
    subarea_recent_bt,
    subarea_full_bt
  ) |>
  dplyr::inner_join(
    dplyr::bind_rows(
      subarea_baseline_sst,
      subarea_recent_sst,
      subarea_full_sst
    )
  ) |>
  dplyr::select(
    AREA_NAME, 
    PERIOD,
    MIN_YEAR, 
    MAX_YEAR,
    MEAN_GEAR_TEMPERATURE, 
    SD_GEAR_TEMPERATURE, 
    MIN_GEAR_TEMPERATURE, 
    MAX_GEAR_TEMPERATURE, 
    MEAN_SURFACE_TEMPERATURE,
    MIN_SURFACE_TEMPERATURE, 
    MAX_SURFACE_TEMPERATURE, 
    SD_SURFACE_TEMPERATURE
  )

z_levels <- factor(
  c(paste0("Mean (", range_baseline[1], "–", range_baseline[2], ")"), "\u00B1 1 SD"),
  levels = c(paste0("Mean (", range_baseline[1], "–", range_baseline[2], ")"), "\u00B1 1 SD")
)

year_breaks <- seq(
  plyr::round_any(c(min_year), 2, floor),
  plyr::round_any(c(max_year), 2, floor),
  by = 2
)

year_lab <- year_breaks

year_lab[!(year_lab %in% seq(
  plyr::round_any(c(min_year), 2, floor),
  plyr::round_any(c(max_year), 2, floor),
  by = 4
))] <- ""


p_bt_timeseries <- 
  ggplot() +
  geom_hline(data = subarea_baseline_bt,
             mapping = aes(
               yintercept = MEAN_GEAR_TEMPERATURE, 
               linetype = z_levels[1]
             ),
             color = "grey50") +
  geom_hline(data = subarea_baseline_bt,
             mapping = aes(
               yintercept = MEAN_GEAR_TEMPERATURE + SD_GEAR_TEMPERATURE, 
               linetype = z_levels[2]),
             color = "grey50") +
  geom_hline(data = subarea_baseline_bt,
             mapping = aes(yintercept = MEAN_GEAR_TEMPERATURE - SD_GEAR_TEMPERATURE, 
                           linetype = z_levels[2]),
             color = "grey50") +
  geom_point(data = subarea_bt,
             mapping = aes(x = YEAR, y = MEAN_GEAR_TEMPERATURE),
             color = "#0085CA") +
  scale_x_continuous(name = "Year", breaks = year_breaks, labels = year_lab) +
  scale_y_continuous(name = expression('Mean BT ('*degree*C*')')) +
  facet_wrap(~factor(AREA_NAME, levels = subarea_levels)) +
  theme_timeseries_blue_strip() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank())

png(here::here("plots", region, paste0(max_year, "_", region, "_bt_timeseries.png")),
    width = 7, height = 3, units = "in", res = 300)
print(p_bt_timeseries)
dev.off()

# SST time series relative to baseline -------------------------------------------------------------

p_sst_timeseries <- 
  ggplot() +
  geom_hline(data = subarea_baseline_sst,
             mapping = aes(
               yintercept = MEAN_SURFACE_TEMPERATURE, 
               linetype = z_levels[1]
             ),
             color = "grey50") +
  geom_hline(data = subarea_baseline_sst,
             mapping = aes(
               yintercept = MEAN_SURFACE_TEMPERATURE + SD_SURFACE_TEMPERATURE, 
               linetype = z_levels[2]),
             color = "grey50") +
  geom_hline(data = subarea_baseline_sst,
             mapping = aes(yintercept = MEAN_SURFACE_TEMPERATURE - SD_SURFACE_TEMPERATURE, 
                           linetype = z_levels[2]),
             color = "grey50") +
  geom_point(data = subarea_sst,
             mapping = aes(x = YEAR, y = MEAN_SURFACE_TEMPERATURE),
             color = "#0085CA") +
  scale_x_continuous(name = "Year", breaks = year_breaks, labels = year_lab) +
  scale_y_continuous(name = expression('Mean SST ('*degree*C*')')) +
  facet_wrap(~factor(AREA_NAME, levels = subarea_levels)) +
  theme_timeseries_blue_strip() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank())

png(here::here("plots", region, paste0(max_year, "_", region, "_sst_timeseries.png")),
    width = 7, height = 3, units = "in", res = 300)
print(p_sst_timeseries)
dev.off()

sst_bt_legend_position <- switch(
  region, AI = c(0.86, 0.82), GOA = c(0.21, 0.82)
)

sst_bt_plot_width_mm <- switch(
  region, AI = 169, GOA = 120
)

p_sst_bt <- 
  cowplot::plot_grid(
    p_sst_timeseries +
      theme(legend.position = "inside",
            legend.position.inside = sst_bt_legend_position,
            legend.text = element_text(size = 7),
            legend.box = element_blank(),
            legend.key.width = unit(2.5, units = "mm"),
            legend.key.height = unit(1.5, units = "mm"),
            strip.text = element_text(size = 8),
            legend.direction = "horizontal",
            axis.title.y = element_text(size = 8),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_blank()),
    p_bt_timeseries +
      theme(legend.position = "none",
            axis.text = element_text(size = 7),
            axis.title.y = element_text(size = 8),
            strip.text = element_blank(),
            strip.background = element_blank()),
    align = "v",
    nrow = 2
  )

png(here::here("plots", region, paste0(max_year, "_", region, "_sst_bt_timeseries.png")),
    width = sst_bt_plot_width_mm, height = 60, units = "mm", res = 300)
print(p_sst_bt)
dev.off()



# Bottom and surface temperature time series relative to full time series --------------------------

z_level_no_baseline <- factor(
  c("Mean", "\u00B1 1 SD"),
  levels = c("Mean", "\u00B1 1 SD")
)

subarea_no_baseline_bt <- subarea_bt |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::summarise(
    SD_GEAR_TEMPERATURE = sd(MEAN_GEAR_TEMPERATURE),
    MEAN_GEAR_TEMPERATURE = mean(MEAN_GEAR_TEMPERATURE)
  )

subarea_no_baseline_sst <- subarea_sst |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::summarise(
    SD_SURFACE_TEMPERATURE = sd(MEAN_SURFACE_TEMPERATURE),
    MEAN_SURFACE_TEMPERATURE = mean(MEAN_SURFACE_TEMPERATURE)
  )

p_bt_timeseries_no_baseline <- 
  ggplot() +
  geom_hline(data = subarea_no_baseline_bt,
             mapping = aes(
               yintercept = MEAN_GEAR_TEMPERATURE, 
               linetype = z_level_no_baseline[1]
             ),
             color = "grey50") +
  geom_hline(data = subarea_no_baseline_bt,
             mapping = aes(
               yintercept = MEAN_GEAR_TEMPERATURE + SD_GEAR_TEMPERATURE, 
               linetype = z_level_no_baseline[2]),
             color = "grey50") +
  geom_hline(data = subarea_no_baseline_bt,
             mapping = aes(yintercept = MEAN_GEAR_TEMPERATURE - SD_GEAR_TEMPERATURE, 
                           linetype = z_levels[2]),
             color = "grey50") +
  geom_point(data = subarea_bt,
             mapping = aes(x = YEAR, y = MEAN_GEAR_TEMPERATURE),
             color = "#0085CA") +
  scale_x_continuous(name = "Year", breaks = year_breaks, labels = year_lab) +
  scale_y_continuous(name = expression('Mean BT ('*degree*C*')')) +
  facet_wrap(~factor(AREA_NAME, levels = subarea_levels)) +
  theme_timeseries_blue_strip() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank())

png(here::here("plots", region, paste0(max_year, "_", region, "_bt_timeseries_no_baseline.png")),
    width = 7, height = 3, units = "in", res = 300)
print(p_bt_timeseries_no_baseline)
dev.off()

p_sst_timeseries_no_baseline <- 
  ggplot() +
  geom_hline(data = subarea_no_baseline_sst,
             mapping = aes(
               yintercept = MEAN_SURFACE_TEMPERATURE, 
               linetype = z_level_no_baseline[1]
             ),
             color = "grey50") +
  geom_hline(data = subarea_no_baseline_sst,
             mapping = aes(
               yintercept = MEAN_SURFACE_TEMPERATURE + SD_SURFACE_TEMPERATURE, 
               linetype = z_level_no_baseline[2]),
             color = "grey50") +
  geom_hline(data = subarea_no_baseline_sst,
             mapping = aes(yintercept = MEAN_SURFACE_TEMPERATURE - SD_SURFACE_TEMPERATURE, 
                           linetype = z_level_no_baseline[2]),
             color = "grey50") +
  geom_point(data = subarea_sst,
             mapping = aes(x = YEAR, y = MEAN_SURFACE_TEMPERATURE),
             color = "#0085CA") +
  scale_x_continuous(name = "Year", breaks = year_breaks, labels = year_lab) +
  scale_y_continuous(name = expression('Mean SST ('*degree*C*')')) +
  facet_wrap(~factor(AREA_NAME, levels = subarea_levels)) +
  theme_timeseries_blue_strip() +
  theme(legend.position = "bottom",
        axis.title.x = element_blank())

png(here::here("plots", region, paste0(max_year, "_", region, "_sst_timeseries_no_baseline.png")),
    width = 7, height = 3, units = "in", res = 300)
print(p_sst_timeseries_no_baseline)
dev.off()

# Combined SST and BT plot

sst_bt_legend_position <- switch(
  region, AI = c(0.88, 0.82), GOA = c(0.18, 0.82))

p_sst_bt_no_baseline <- 
  cowplot::plot_grid(
    p_sst_timeseries_no_baseline +
      theme(legend.position = "inside",
            legend.position.inside = sst_bt_legend_position,
            legend.text = element_text(size = 7),
            legend.box = element_blank(),
            legend.key.height = unit(1.5, units = "mm"),
            strip.text = element_text(size = 8),
            legend.direction = "horizontal",
            axis.title.y = element_text(size = 8),
            axis.text.y = element_text(size = 7),
            axis.text.x = element_blank()),
    p_bt_timeseries_no_baseline +
      theme(legend.position = "none",
            axis.text = element_text(size = 7),
            axis.title.y = element_text(size = 8),
            strip.text = element_blank(),
            strip.background = element_blank()),
    align = "v",
    nrow = 2
  )

png(here::here("plots", region, paste0(max_year, "_", region, "_sst_bt_no_baseline.png")),
    width = 169*length(subarea_levels)/3, height = 60, units = "mm", res = 300)
print(p_sst_bt_no_baseline)
dev.off()


# Maps ---------------------------------------------------------------------------------------------


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
  filename = here::here("plots", region, paste0(max_year, "_", region, "_bt_anomaly_rel_baseline.png")),
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
  filename = here::here("plots", region, paste0(max_year, "_", region, "_bt_anomaly_full.png")),
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
    y = max(bt_breaks[!is.infinite(bt_breaks)])*0.95, 
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
  theme(legend.position = "none",
        axis.text = element_text(size = 7))

ragg::agg_png(filename = here::here("plots", region, paste0(max_year, "_", region, "_bt_annual_maps.png")),
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
  filename = here::here("plots", region, paste0(max_year, "_", region, "_bottom_temperature_map.png")), 
  width = 6, 
  height = 7.2, 
  units = "in", 
  res = fig_res
)
print(
  cowplot::plot_grid(
    plot_four_panel_map + theme(strip.text = element_text(size = 11), plot.margin = unit(c(5,-5,5,5), units = "pt")),
    bt_map_cbar + theme(plot.margin = unit(c(5,5,5,-15), units = "pt")),
    ncol = 2,
    rel_widths = c(0.8,0.11)
  )
)
dev.off()

## Contrast BT years -------------------------------------------------------------------------------
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
  filename = here::here("plots", region, paste0(max_year, "_", region, "_bt_anomaly_rel_baseline.png")),
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
  filename = here::here("plots", region, paste0(max_year, "_", region, "_bt_anomaly_full.png")),
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

ragg::agg_png(filename = here::here("plots", region, paste0(max_year, "_", region, "_bt_annual_maps.png")),
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
  filename = here::here("plots", region, paste0(max_year, "_", region, "_bottom_temperature_map.png")), 
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

## Surface temperatures ----------------------------------------------------------------------------

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
  filename = here::here("plots", region, paste0(max_year, "_", region, "_sst_anomaly_rel_baseline.png")),
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
  filename = here::here("plots", region, paste0(max_year, "_", region, "_sst_anomaly_full.png")),
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
    y = max(temp_breaks_sst[!is.infinite(temp_breaks_sst)]), 
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
  scale_x_continuous(limits = map_layers$plot.boundary$x,
                     breaks = map_layers$lon.breaks) +
  scale_y_continuous(limits = map_layers$plot.boundary$y,
                     breaks = map_layers$lat.breaks) +
  ggplot2::scale_fill_manual(values = viridis_pal(option = viridis_palette)(n_temp_breaks_sst),
                             drop = FALSE) +
  facet_wrap(~year, nrow = 4) +
  coldpool::theme_multi_map_blue_strip() +
  theme(legend.position = "none")


ragg::agg_png(
  filename = here::here("plots", region, paste0(max_year, "_", region, "_sst_map.png")), 
  width = 6, 
  height = 7.2, 
  units = "in", 
  res = fig_res
)
print(
  cowplot::plot_grid(
    plot_four_panel_map_sst + theme(strip.text = element_text(size = 11), plot.margin = unit(c(5,-5,5,5), units = "pt")),
    temp_map_cbar_sst + theme(plot.margin = unit(c(5,5,5,-15), units = "pt")),
    ncol = 2,
    rel_widths = c(0.8,0.2)
  )
)
dev.off()
