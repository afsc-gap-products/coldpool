library(coldpool)

# Plot GOA or AI data
survey_definition_id <- 47 # GOA
# survey_definition_id <- 52 # AI

# Setup variables for analysis
if(survey_definition_id == 47) {
  region <- "GOA"
  min_year <- 1993 # First year with temperature data from every haul
  max_year <- 2025 # Most recent survey
  range_baseline <- c(1993, 2014)
  year_breaks <- seq(min_year, max_year, 4)
  subarea_levels <- c("Western Gulf of Alaska", "Eastern Gulf of Alaska") # Panel/timeseries order
  temp_by_year <- coldpool::goa_mean_temperature
}

if(survey_definition_id == 52) {
  region <- "AI"
  min_year <- 1991
  max_year <- 2024 # Most recent survey
  range_baseline <- c(1991, 2012)
  year_breaks <- seq(min_year-1, max_year, 4)
  subarea_levels <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians") # Panel/timeseries order
  temp_by_year <- coldpool::ai_mean_temperature
}

subarea_mean <- 
  temp_by_year |>
  dplyr::group_by(SUBAREA) |>
  dplyr::summarise(
    MEAN_TEMPERATURE_5M = mean(MEAN_TEMPERATURE_5M, na.rm = TRUE),
    MEAN_TEMPERATURE_100M = mean(MEAN_TEMPERATURE_100M, na.rm = TRUE),
    MEAN_TEMPERATURE_200M = mean(MEAN_TEMPERATURE_200M, na.rm = TRUE),
    MEAN_GEAR_TEMPERATURE = mean(MEAN_GEAR_TEMPERATURE, na.rm = TRUE),
    MEAN_SURFACE_TEMPERATURE = mean(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE),
    N_5M = sum(N_5M),
    N_100M = sum(N_100M),
    N_200M = sum(N_200M),
    N_GEAR_TEMPERATURE = sum(N_GEAR_TEMPERATURE),
    N_SURFACE_TEMPERATURE = sum(N_SURFACE_TEMPERATURE)
  )

# Means and standard deviations at depth
subarea_mean_baseline <- 
  temp_by_year |>
  dplyr::filter(YEAR >= range_baseline[1] & YEAR <= range_baseline[2]) |>
  dplyr::group_by(SUBAREA) |>
  dplyr::summarise(
    HIST_T_5M = mean(MEAN_TEMPERATURE_5M, na.rm = TRUE),
    HIST_T_100M = mean(MEAN_TEMPERATURE_100M, na.rm = TRUE),
    HIST_T_200M = mean(MEAN_TEMPERATURE_200M, na.rm = TRUE),
    HIST_T_GEAR = mean(MEAN_GEAR_TEMPERATURE, na.rm = TRUE),
    HIST_T_SURFACE = mean(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE),
    HIST_T_SD_5M = sd(MEAN_TEMPERATURE_5M, na.rm = TRUE),
    HIST_T_SD_100M = sd(MEAN_TEMPERATURE_100M, na.rm = TRUE),
    HIST_T_SD_200M  = sd(MEAN_TEMPERATURE_200M, na.rm = TRUE),
    HIST_T_SD_GEAR = sd(MEAN_GEAR_TEMPERATURE, na.rm = TRUE),
    HIST_T_SD_SURFACE = sd(MEAN_SURFACE_TEMPERATURE, na.rm = TRUE),
    HIST_T_PLUS1SD_5M = HIST_T_5M + HIST_T_SD_5M,
    HIST_T_PLUS1SD_100M = HIST_T_100M + HIST_T_SD_100M,
    HIST_T_PLUS1SD_200M = HIST_T_200M + HIST_T_SD_200M,
    HIST_T_PLUS1SD_GEAR = HIST_T_GEAR + HIST_T_SD_GEAR,
    HIST_T_PLUS1SD_SURFACE = HIST_T_SURFACE + HIST_T_SD_SURFACE,
    HIST_T_MINUS1SD_5M = HIST_T_5M - HIST_T_SD_5M,
    HIST_T_MINUS1SD_100M = HIST_T_100M - HIST_T_SD_100M,
    HIST_T_MINUS1SD_200M = HIST_T_200M - HIST_T_SD_200M,
    HIST_T_MINUS1SD_GEAR = HIST_T_GEAR - HIST_T_SD_GEAR,
    HIST_T_MINUS1SD_SURFACE = HIST_T_SURFACE - HIST_T_SD_SURFACE,
    HIST_T_PLUS2SD_5M = HIST_T_5M + 2*HIST_T_SD_5M,
    HIST_T_PLUS2SD_100M = HIST_T_100M + 2*HIST_T_SD_100M,
    HIST_T_PLUS2SD_200M = HIST_T_200M + 2*HIST_T_SD_200M,
    HIST_T_PLUS2SD_GEAR = HIST_T_GEAR + 2*HIST_T_SD_GEAR,
    HIST_T_PLUS2SD_SURFACE = HIST_T_SURFACE + 2*HIST_T_SD_SURFACE,
    HIST_T_MINUS2SD_5M = HIST_T_5M - 2*HIST_T_SD_5M,
    HIST_T_MINUS2SD_100M = HIST_T_100M - 2*HIST_T_SD_100M,
    HIST_T_MINUS2SD_200M = HIST_T_200M - 2*HIST_T_SD_200M,
    HIST_T_MINUS2SD_GEAR = HIST_T_GEAR - 2*HIST_T_SD_GEAR,
    HIST_T_MINUS2SD_SURFACE = HIST_T_SURFACE - 2*HIST_T_SD_SURFACE,
    HIST_N_5M = sum(N_5M),
    HIST_N_100M = sum(N_100M),
    HIST_N_200M = sum(N_200M),
    HIST_N_GEAR_TEMPERATURE = sum(N_GEAR_TEMPERATURE),
    HIST_N_SURFACE_TEMPERATURE = sum(N_SURFACE_TEMPERATURE)
  )

# Absolute difference in subarea means and Z-score anomalies
temp_anomaly <-
  temp_by_year |>
  dplyr::inner_join(subarea_mean_baseline, by = "SUBAREA") |>
  dplyr::mutate(
    DIFF_T_5M = MEAN_TEMPERATURE_5M - HIST_T_5M,
    DIFF_T_100M = MEAN_TEMPERATURE_100M - HIST_T_100M,
    DIFF_T_200M = MEAN_TEMPERATURE_200M - HIST_T_200M,
    ZSCORE_T_5M = DIFF_T_5M/HIST_T_SD_5M,
    ZSCORE_T_100M = DIFF_T_100M/HIST_T_SD_100M,
    ZSCORE_T_200M = DIFF_T_200M/HIST_T_SD_200M,
  )

# Plot time series ---------------------------------------------------------------------------------

ggplot() +
  geom_hline(
    data = subarea_mean, 
    mapping = aes(
      yintercept = MEAN_TEMPERATURE_5M, 
      color = "1-5 m", 
      linetype = paste0("Mean (", min_year, "-", max_year, ")")
    ),
  ) +
  geom_point(
    data = temp_by_year,
    mapping = aes(
      x = YEAR, 
      y = MEAN_TEMPERATURE_5M, 
      color = "1-5 m"
    )
  ) +
  geom_hline(
    data = subarea_mean, 
    mapping = aes(
      yintercept = MEAN_TEMPERATURE_200M, 
      color = "195-205 m", 
      linetype = paste0("Mean (", min_year, "-", max_year, ")")
    )) +
  geom_point(
    data = temp_by_year,
    mapping = aes(x = YEAR, y = MEAN_TEMPERATURE_200M, color = "195-205 m")
  ) +
  scale_x_continuous(name = "Year", breaks = year_breaks) +
  scale_y_continuous(name = expression(bold('Temperature ('*degree*C*')'))) +
  scale_linetype_manual(values = 2) +
  scale_color_manual(values = c("1-5 m" =  "#0071ff", "195-205 m" = "#000040")) +
  facet_wrap(~SUBAREA) +
  theme_timeseries_blue_strip()


ggplot() +
  geom_hline(
    data = subarea_mean_baseline, 
    mapping = aes(
      yintercept = HIST_T_5M, 
      color = "1-5 m", 
      linetype = paste0("Mean (", range_baseline[1], "-", range_baseline[2], ")")
    ),
  ) +
  geom_point(
    data = temp_by_year,
    mapping = aes(
      x = YEAR, 
      y = MEAN_TEMPERATURE_5M, 
      color = "1-5 m"
    )
  ) +
  geom_hline(
    data = subarea_mean_baseline, 
    mapping = aes(
      yintercept = HIST_T_200M, 
      color = "195-205 m", 
      linetype = paste0("Mean (", range_baseline[1], "-", range_baseline[2], ")")
    )) +
  geom_point(
    data = temp_by_year,
    mapping = aes(x = YEAR, y = MEAN_TEMPERATURE_200M, color = "195-205 m")
  ) +
  scale_x_continuous(name = "Year", breaks = year_breaks) +
  scale_y_continuous(name = expression(bold('Temperature ('*degree*C*')'))) +
  scale_linetype_manual(values = 2) +
  scale_color_manual(values = c("1-5 m" =  "#0071ff", "195-205 m" = "#000040")) +
  facet_wrap(~SUBAREA) +
  theme_timeseries_blue_strip()