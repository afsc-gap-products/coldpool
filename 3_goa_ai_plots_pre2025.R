# Explore plotting options
library(coldpool)
library(tidyterra)
library(spmodel)

fig_res <- 300

survey_definition_id <- 52

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
sst <- readRDS(here::here("output", paste0(region, "_sst.rds")))


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


temperature_time_series <- 
  dplyr::full_join(subarea_bt, subarea_sst) |>
  dplyr::select(YEAR, AREA_NAME, MEAN_GEAR_TEMPERATURE, MEAN_SURFACE_TEMPERATURE)

temperature_time_series |>
  dplyr::mutate(MEAN_GEAR_TEMPERATURE = round(MEAN_GEAR_TEMPERATURE, 2),
                MEAN_SURFACE_TEMPERATURE = round(MEAN_SURFACE_TEMPERATURE, 2)) |>
  
  dplyr::arrange(factor(AREA_NAME, levels = subarea_levels), YEAR) |>
  write.csv(
    file = here::here("output", paste0(region, "_yearly_ecoregion_temperature.csv")),
    row.names = FALSE
  )


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

write.csv(
  mean_temperature_by_period,
  file = here::here("plots", paste0(region, "_temperature_by_period.csv"))
)


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

png(here::here("plots", region, paste0(sel_year, "_", region, "_bt_timeseries.png")),
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

png(here::here("plots", region, paste0(sel_year, "_", region, "_sst_timeseries.png")),
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

png(here::here("plots", region, paste0(sel_year, "_", region, "_sst_bt_timeseries.png")),
    width = sst_bt_plot_width_mm, height = 60, units = "mm", res = 300)
print(p_sst_bt)
dev.off()



# Bottom and surface temperature time series relative to full time series --------------------------

subarea_baseline_bt <- subarea_bt |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::summarise(
    SD_GEAR_TEMPERATURE = sd(MEAN_GEAR_TEMPERATURE),
    MEAN_GEAR_TEMPERATURE = mean(MEAN_GEAR_TEMPERATURE)
  )

subarea_baseline_sst <- subarea_sst |>
  dplyr::group_by(AREA_NAME) |>
  dplyr::summarise(
    SD_SURFACE_TEMPERATURE = sd(MEAN_SURFACE_TEMPERATURE),
    MEAN_SURFACE_TEMPERATURE = mean(MEAN_SURFACE_TEMPERATURE)
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

png(here::here("plots", region, paste0(sel_year, "_", region, "_bt_timeseries_no_baseline.png")),
    width = 7, height = 3, units = "in", res = 300)
print(p_bt_timeseries_no_baseline)
dev.off()

p_sst_timeseries_no_baseline <- 
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

png(here::here("plots", region, paste0(sel_year, "_", region, "_sst_timeseries_no_baseline.png")),
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

png(here::here("plots", region, paste0(sel_year, "_", region, "_sst_bt_no_baseline.png")),
    width = 169*length(subarea_levels)/3, height = 60, units = "mm", res = 300)
print(p_sst_bt_no_baseline)
dev.off()