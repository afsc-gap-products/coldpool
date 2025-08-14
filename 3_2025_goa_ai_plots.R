library(coldpool)

# Select which region to plot (GOA or AI)
# survey_definition_id <- 47 # GOA
survey_definition_id <- 52 # AI

# Setup variables for analysis
if(survey_definition_id == 47) {
  temp_by_year <- coldpool::goa_mean_temperature
  region <- "GOA"
  min_year <- 1993 # First year with temperature data from every haul
  max_year <- 2025 # Most recent survey
  range_baseline <- c(1993, 2014)
  subarea_levels <- c("Western Gulf of Alaska", "Eastern Gulf of Alaska") # Panel/timeseries order
  point_colors <- c("Surface" =  "#0071ff", "200 m" = "#000040")
}

if(survey_definition_id == 52) {
  temp_by_year <- coldpool::ai_mean_temperature
  region <- "AI"
  min_year <- 1991
  max_year <- 2024 # Most recent survey
  range_baseline <- c(1991, 2012)
  subarea_levels <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians") # Panel/timeseries order
  point_colors <- c("Surface" =  "#0071ff", "Bottom" = "#000040")
}

subarea_mean <- 
  temp_by_year |>
  dplyr::group_by(SUBAREA) |>
  dplyr::summarise(
    MEAN_200M_TEMPERATURE = mean(MEAN_200M_TEMPERATURE, na.rm = TRUE),
    MEAN_GEAR_TEMPERATURE = mean(MEAN_GEAR_TEMPERATURE, na.rm = TRUE),
    MEAN_SURFACE_TEMPERATURE = mean(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE)
  )

# Means and standard deviations at depth
subarea_mean_baseline <- 
  temp_by_year |>
  dplyr::filter(YEAR >= range_baseline[1] & YEAR <= range_baseline[2]) |>
  dplyr::group_by(SUBAREA) |>
  dplyr::summarise(
    HIST_T_200M = mean(MEAN_200M_TEMPERATURE, na.rm = TRUE),
    HIST_T_GEAR = mean(MEAN_GEAR_TEMPERATURE, na.rm = TRUE),
    HIST_T_SURFACE = mean(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE),
    HIST_T_SD_200M  = sd(MEAN_200M_TEMPERATURE, na.rm = TRUE),
    HIST_T_SD_GEAR = sd(MEAN_GEAR_TEMPERATURE, na.rm = TRUE),
    HIST_T_SD_SURFACE = sd(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE),
    HIST_T_PLUS1SD_200M = HIST_T_200M + HIST_T_SD_200M,
    HIST_T_PLUS1SD_GEAR = HIST_T_GEAR + HIST_T_SD_GEAR,
    HIST_T_PLUS1SD_SURFACE = HIST_T_SURFACE + HIST_T_SD_SURFACE,
    HIST_T_MINUS1SD_200M = HIST_T_200M - HIST_T_SD_200M,
    HIST_T_MINUS1SD_GEAR = HIST_T_GEAR - HIST_T_SD_GEAR,
    HIST_T_MINUS1SD_SURFACE = HIST_T_SURFACE - HIST_T_SD_SURFACE
  )
  

# Absolute difference in subarea means and Z-score anomalies
temp_anomaly <-
  temp_by_year |>
  dplyr::inner_join(subarea_mean_baseline, by = "SUBAREA") |>
  dplyr::mutate(
    DIFF_T_200M = MEAN_200M_TEMPERATURE - HIST_T_200M,
    DIFF_T_GEAR = MEAN_GEAR_TEMPERATURE - HIST_T_GEAR,
    DIFF_T_SURFACE = MEAN_SURFACE_TEMPERATURE - HIST_T_SURFACE,
    ZSCORE_T_200M = DIFF_T_200M/HIST_T_SD_200M,
    ZSCORE_T_SURFACE = DIFF_T_GEAR/HIST_T_SD_GEAR,
    ZSCORE_T_GEAR = DIFF_T_SURFACE/HIST_T_SD_SURFACE
  ) |>
  dplyr::select(SUBAREA, YEAR, DIFF_T_200M, DIFF_T_GEAR, DIFF_T_SURFACE, ZSCORE_T_200M, ZSCORE_T_SURFACE, ZSCORE_T_GEAR)

# Long format data frame for multipanel time series plots
temp_by_year_long <- 
  temp_by_year |>
  dplyr::select(
    SUBAREA, 
    YEAR, 
    MEAN_200M_TEMPERATURE, 
    MEAN_SURFACE_TEMPERATURE, 
    MEAN_GEAR_TEMPERATURE
  ) |>
  tidyr::pivot_longer(cols = c("MEAN_200M_TEMPERATURE", "MEAN_SURFACE_TEMPERATURE", "MEAN_GEAR_TEMPERATURE"),
                      names_to = "DEPTH", 
                      values_to = "MEAN_TEMPERATURE") |>
  dplyr::mutate(
    DEPTH = 
      dplyr::case_match(
        DEPTH,
        "MEAN_200M_TEMPERATURE" ~ "200 m",
        "MEAN_SURFACE_TEMPERATURE" ~ "Surface",
        "MEAN_GEAR_TEMPERATURE" ~ "Bottom"
      )
  ) |>
  dplyr::inner_join(
    temp_by_year |>
      dplyr::select(SUBAREA, YEAR, SE_200M_TEMPERATURE, SE_SURFACE_TEMPERATURE, SE_GEAR_TEMPERATURE) |>
      tidyr::pivot_longer(cols = c("SE_200M_TEMPERATURE", "SE_SURFACE_TEMPERATURE", "SE_GEAR_TEMPERATURE"),
                          names_to = "DEPTH", 
                          values_to = "SE") |>
      dplyr::mutate(
        DEPTH = 
          dplyr::case_match(
            DEPTH,
            "SE_200M_TEMPERATURE" ~ "200 m",
            "SE_SURFACE_TEMPERATURE" ~ "Surface",
            "SE_GEAR_TEMPERATURE" ~ "Bottom"
          )
      )
  ) |>
  dplyr::filter(DEPTH %in% names(point_colors))


# Historical baseline temperatures for time series plots
subarea_mean_baseline_long <- 
  subarea_mean_baseline |>
  dplyr::select(
    SUBAREA, 
    HIST_T_200M, 
    HIST_T_PLUS1SD_200M, 
    HIST_T_MINUS1SD_200M, 
    HIST_T_SURFACE, 
    HIST_T_PLUS1SD_SURFACE, 
    HIST_T_MINUS1SD_SURFACE,
    HIST_T_GEAR, 
    HIST_T_PLUS1SD_GEAR, 
    HIST_T_MINUS1SD_GEAR
  ) |>
  tidyr::pivot_longer(cols = 2:10,
                      names_to = "DEPTH", 
                      values_to = "TEMPERATURE") |>
  dplyr::mutate(
    ANOMALY = ifelse(
      stringr::str_detect(DEPTH, "PLUS"), "PLUS1SD", 
      ifelse(stringr::str_detect(DEPTH, "MINUS"), "MINUS1SD", "MEAN")
    ),
    DEPTH = ifelse(
      stringr::str_detect(DEPTH, "200M"), 
      "200 m", 
      ifelse(stringr::str_detect(DEPTH, "SURFACE"), "Surface", "Bottom")
    )
  ) |>
  dplyr::filter(DEPTH %in% names(point_colors))

temp_anomaly_long <- 
  temp_anomaly |>
  dplyr::select(
    SUBAREA, 
    YEAR,
    DIFF_T_SURFACE,
    DIFF_T_GEAR,
    DIFF_T_200M
  ) |>
  tidyr::pivot_longer(cols = c("DIFF_T_SURFACE", "DIFF_T_GEAR", "DIFF_T_200M"),
                      names_to = "DEPTH", 
                      values_to = "DELTA_T") |>
  dplyr::mutate(
    DEPTH = ifelse(
      stringr::str_detect(DEPTH, "200M"), 
      "200 m", 
      ifelse(stringr::str_detect(DEPTH, "SURFACE"), "Surface", "Bottom")
    )
  ) |>
  dplyr::filter(DEPTH %in% names(point_colors))

# Plot time series ---------------------------------------------------------------------------------

# Setup year breaks and labels

start_year <- min(temp_by_year$YEAR) - min(temp_by_year$YEAR)%%4
end_year <- max(temp_by_year$YEAR)
year_breaks <- year_labels <- seq(start_year, end_year, by = 2)
year_labels[year_labels %% 4 > 0] <- ""


# Two panel time series plot w/ historical mean
p_subarea_ts <- 
  ggplot() +
  geom_hline(data = 
               dplyr::filter(
                 subarea_mean_baseline_long, 
                 ANOMALY == "MEAN"
               ),
             mapping = 
               aes(
                 yintercept = TEMPERATURE,
                 color = DEPTH,
                 linetype = paste0("Mean (",paste(range_baseline, collapse = "-"), ")")
               )
  ) +
  geom_point(data = temp_by_year_long, 
             mapping = aes(x = YEAR, y = MEAN_TEMPERATURE, color = DEPTH)) +
  geom_errorbar(data = temp_by_year_long, 
                mapping = aes(x = YEAR, ymin = MEAN_TEMPERATURE - 2*SE, ymax = MEAN_TEMPERATURE + 2*SE, color = DEPTH),
                width = 0) +
  scale_x_continuous(name = "Year", breaks = year_breaks, labels = year_labels) +
  scale_y_continuous(name = expression(bold('Temperature ('*degree*C*')'))) +
  scale_linetype_manual(values = 1) +
  scale_color_manual(values = point_colors) +
  facet_wrap(~SUBAREA) +
  theme_timeseries_blue_strip()

# Time series by depth and subarea and historical +/- 1SD lines
p_subarea_depth_ts <-
  ggplot() +
  geom_hline(data = 
               dplyr::filter(
                 subarea_mean_baseline_long, 
                 ANOMALY == "MEAN"
               ),
             mapping = 
               aes(
               yintercept = TEMPERATURE,
               linetype = paste0("Mean (",paste(range_baseline, collapse = "-"), ")")
               ),
             color = "grey50") +
  geom_hline(data = 
               dplyr::filter(
                 subarea_mean_baseline_long, 
                 ANOMALY %in% c("PLUS1SD", "MINUS1SD")
               ),
             mapping = 
               aes(
                 yintercept = TEMPERATURE,
                           linetype = paste0("Mean \u00B1 1 SD (",paste(range_baseline, collapse = "-"), ")")
                 ),
             color = "grey50") +
  geom_errorbar(data = temp_by_year_long, 
                mapping = aes(x = YEAR, ymin = MEAN_TEMPERATURE - 2*SE, ymax = MEAN_TEMPERATURE + 2*SE, color = DEPTH),
                width = 0) +
  geom_point(data = temp_by_year_long, 
             mapping = aes(x = YEAR, y = MEAN_TEMPERATURE, color = DEPTH)) +
  scale_x_continuous(name = "Year", breaks = year_breaks, labels = year_labels) +
  scale_y_continuous(name = expression(bold('Mean temperature ('*degree*C*')'))) +
  scale_color_manual(values = point_colors, guide = "none") +
  facet_grid(
    factor(DEPTH, levels = names(point_colors)) ~ 
      SUBAREA, 
    scales = "free_y"
  ) +
  theme_timeseries_blue_strip()

# Delta T time series by depth and subarea
# Delta T = observed temperature - historical mean
p_delta_t <- 
  ggplot() +
  geom_hline(yintercept = 0,
             linetype = 1,
             color = "grey50") +
  geom_point(data = temp_anomaly_long, 
             mapping = aes(x = YEAR, y = DELTA_T, color = DEPTH)) +
  scale_x_continuous(name = "Year", breaks = year_breaks, labels = year_labels) +
  scale_y_continuous(name = expression(bold(Delta*'T ('*degree*C*')'))) +
  scale_color_manual(values = point_colors) +
  facet_grid(
    factor(DEPTH, levels = names(point_colors)) ~ 
      SUBAREA, 
    scales = "free_y"
  ) +
  theme_timeseries_blue_strip() +
  theme(legend.position = "none")

png(
  filename = here::here("plots", region, paste0(max_year, "_temperature_by_subarea.png")),
  width = 169,
  height = 80,
  units = "mm",
  res = 300
)
print(p_subarea_ts)
dev.off()

png(
  filename = here::here("plots", region, paste0(max_year, "_temperature_by_subarea_depth.png")),
  width = 140,
  height = 140,
  units = "mm",
  res = 300
)
print(p_subarea_depth_ts)
dev.off()

png(
  filename = here::here("plots", region, paste0(max_year, "_delta_t_by_subarea_depth.png")),
  width = 120,
  height = 120,
  units = "mm",
  res = 300
)
print(p_delta_t)
dev.off()

# Make a csv temperature table
write.csv(
  temp_by_year,
  here::here("plots", region, paste0(max_year, "_subarea_temperature_table.csv")),
  row.names = FALSE
)
