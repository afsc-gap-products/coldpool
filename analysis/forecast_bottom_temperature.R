library(coldpool)
library(dplyr)
library(rlang)
library(mgcv)
library(cowplot)

# Set to Mar, Apr, or May
prediction_month <- "Mar"

# Current year 
current_year <- 2025

# Sea ice extent data from NSIDC (https://www.ncei.noaa.gov/access/monitoring/regional-sea-ice/area/Bering/3)
sea_ice_extent <- 
  dplyr::bind_rows(
    read.csv(file = here::here("analysis", "nsidc_mar_2025.csv"), skip = 3) |>
      dplyr::mutate(Mon = "Mar"),
    read.csv(file = here::here("analysis", "nsidc_apr_2025.csv"), skip = 3) |>
      dplyr::mutate(Mon = "Apr"),
    read.csv(file = here::here("analysis", "nsidc_feb_2025.csv"), skip = 3) |>
      dplyr::mutate(Mon = "Feb")
  ) |>
  dplyr::mutate(YEAR = floor(Date/100))

cpi_sea_ice <- 
  dplyr::inner_join(coldpool::cold_pool_index, sea_ice_extent)

reg_labels <- cpi_sea_ice %>%
  group_by(Mon) %>%
  group_modify(~ {
    model <- gam(AREA_LTE2_KM2 ~ s(Anomaly), data = .x)
    r2 <- summary(model)$r.sq
    tibble(label = paste0("R² = ", round(r2, 2)))
  }) %>%
  ungroup()

reg_labels <- reg_labels %>%
  mutate(
    x = min(cpi_sea_ice$Anomaly, na.rm = TRUE),  # adjust as needed
    y = max(cpi_sea_ice$AREA_LTE2_KM2, na.rm = TRUE)
  )

ggplot() +
  geom_point(data = cpi_sea_ice,
             mapping = aes(x = Anomaly, y = AREA_LTE2_KM2)) +
  geom_smooth(data = cpi_sea_ice,
              mapping = aes(x = Anomaly, y = AREA_LTE2_KM2),
              method = "gam",
              formula = y ~ s(x)) +
  geom_text(data = reg_labels,
            mapping = aes(x = x, y = y, label = label),
            hjust = 0, size = 3.5) +
  scale_y_continuous(name = expression('Summer BTS cold pool extent ('*km^2*')')) +
  scale_x_continuous(name = "EBS Sea Ice Anomaly") +
  facet_grid(~factor(Mon, levels = c("Feb", "Mar", "Apr"))) +
  theme_bw()

library(ggplot2)
library(dplyr)
library(mgcv)
library(rlang)

plot_gam_facets <- function(data, predictor, response, facet_var,
                            facet_levels = NULL, label_y = NULL, label_position = "top-right") {
  predictor_sym <- sym(predictor)
  response_sym <- sym(response)
  facet_sym <- sym(facet_var)
  
  # Determine facet level order
  facet_order <- if (!is.null(facet_levels)) {
    facet_levels
  } else {
    unique(pull(data, !!facet_sym))
  }
  
  # Convert facet variable to a factor for consistent facet order
  data <- data %>%
    mutate(facet_var_factor = factor(!!facet_sym, levels = facet_order))
  
  # Step 1: Calculate R² and label position per group
  reg_labels <- data %>%
    group_by(!!facet_sym) %>%
    group_modify(~ {
      formula <- reformulate(paste0("s(", predictor, ")"), response)
      model <- gam(formula, data = .x)
      r2 <- summary(model)$r.sq
      
      # Determine label x-position based on user's choice
      label_x <- if (label_position == "top-left") {
        min(.x[[predictor]], na.rm = TRUE)  # min value of predictor for top-left
      } else {
        max(.x[[predictor]], na.rm = TRUE)  # max value of predictor for top-right
      }
      
      tibble(
        label = paste0("R² = ", round(r2, 2)),
        x = label_x
      )
    }) %>%
    ungroup() %>%
    mutate(
      y = if (!is.null(label_y)) label_y else max(pull(data, !!response_sym), na.rm = TRUE),
      facet_var_factor = factor(pull(., !!facet_sym), levels = facet_order)
    )
  
  # Step 2: Plot
  ggplot(data, aes(x = !!predictor_sym, y = !!response_sym)) +
    geom_point() +
    geom_smooth(method = "gam", formula = y ~ s(x)) +
    geom_text(data = reg_labels,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE,
              hjust = ifelse(label_position == "top-left", 0, 1),  # Left for top-left, right for top-right
              size = 3.5) +
    facet_grid(~facet_var_factor, scales = "free_x") +
    theme_bw()
}

plot_gam_facets(
  data = cpi_sea_ice,
  predictor = "Anomaly",
  response = "AREA_LTE2_KM2",
  facet_var = "Mon",
  facet_levels = c("Feb", "Mar", "Apr"),
  label_position = "top-left"
) +  
  scale_y_continuous(name = expression('Summer BTS cold pool extent ('*km^2*')')) +
  scale_x_continuous(name = "EBS Sea Ice Anomaly")
  

plot_gam_facets(
  data = cpi_sea_ice,
  predictor = "Anomaly",
  response = "MEAN_GEAR_TEMPERATURE",
  facet_var = "Mon",
  facet_levels = c("Feb", "Mar", "Apr")
) + 
  scale_y_continuous(name = expression('Summer BTS bottom temperature ('*degree*C*')')) +
  scale_x_continuous(name = "NSIDC-EBS Sea Ice Anomaly")

# Fit GAMs
sea_ice_gam <- mgcv::gam(
  formula = AREA_LTE2_KM2 ~ s(Anomaly, bs = "tp"), 
          data = dplyr::filter(cpi_sea_ice, Mon == prediction_month)
  )

bt_gam <- mgcv::gam(
  formula = MEAN_GEAR_TEMPERATURE ~ s(Anomaly, bs = "tp"), 
  data = dplyr::filter(cpi_sea_ice, Mon == prediction_month)
)

predictions <- dplyr::filter(sea_ice_extent, Mon == prediction_month, YEAR == current_year)
predictions$AREA_LTE2_KM2 <- predict(sea_ice_gam, newdata = predictions)
predictions$AREA_LTE2_KM2_SE <- predict(sea_ice_gam, newdata = predictions, se.fit = TRUE)$se

predictions$MEAN_GEAR_TEMPERATURE <- predict(bt_gam, newdata = predictions)
predictions$MEAN_GEAR_TEMPERATURE_SE <- predict(bt_gam, newdata = predictions, se.fit = TRUE)$se

timeseries <- dplyr::filter(cpi_sea_ice, Mon == prediction_month)


p1 <- 
  ggplot() +
  geom_bar(data = timeseries,
           mapping = aes(x = YEAR, y = AREA_LTE2_KM2, fill = "Observed"),
           stat = "identity") +
  geom_bar(data = predictions,
           mapping = aes(x = YEAR, y = AREA_LTE2_KM2, fill = "Forecasted"),
           stat = "identity") +
  geom_errorbar(data = predictions,
                mapping = aes(
                  x = YEAR, 
                  ymin = AREA_LTE2_KM2-2*AREA_LTE2_KM2_SE, 
                  ymax = AREA_LTE2_KM2+2*AREA_LTE2_KM2_SE,
                  width = 0
                  ),
                stat = "identity") +
  geom_hline(yintercept = mean(timeseries$AREA_LTE2_KM2), linetype = 2) +
  scale_fill_manual(values = c("Observed" = "#0055a4", "Forecasted" = "salmon")) +
  scale_y_continuous(name = expression(bold('Cold pool extent ('*km^2*')'))) +
  scale_x_continuous(name = "Year") +
  facet_wrap(~"Forecasted cold pool extent") +
  theme_bw() +
  theme(axis.title = element_text(color = "black", face = "bold"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "inside",
        legend.text = element_text(size = 8),
        legend.key.height = unit(3, units = "mm"),
        legend.key.width = unit(3, units = "mm"),
        legend.position.inside = c(0.12, 0.87),
        strip.text = element_text(size = 9,
                                  color = "white",
                                  face = "bold",
                                  margin = margin(0.5, 0, 0.5, 0, "mm")),
        strip.background = element_rect(fill = "#0055a4",
                                        color = NA))
p2 <- 
  ggplot() +
  geom_bar(data = dplyr::filter(cpi_sea_ice, Mon == prediction_month),
           mapping = aes(x = YEAR, y = MEAN_GEAR_TEMPERATURE, fill = "Observed"),
           stat = "identity") +
  geom_bar(data = predictions,
           mapping = aes(x = YEAR, y = MEAN_GEAR_TEMPERATURE, fill = "Forecasted"),
           stat = "identity") +
  geom_errorbar(data = predictions,
                mapping = aes(
                  x = YEAR, 
                  ymin = MEAN_GEAR_TEMPERATURE-2*MEAN_GEAR_TEMPERATURE_SE, 
                  ymax = MEAN_GEAR_TEMPERATURE+2*MEAN_GEAR_TEMPERATURE_SE,
                  width = 0
                ),
                stat = "identity") +
  geom_hline(yintercept = mean(timeseries$MEAN_GEAR_TEMPERATURE), linetype = 2) +
  scale_fill_manual(values = c("Observed" = "#0055a4", "Forecasted" = "salmon")) +
  scale_y_continuous(name = expression(bold('Mean bottom temperature ('*degree*C*')'))) +
  scale_x_continuous(name = "Year") +
  facet_wrap(~"Forecasted mean bottom temperature") +
  theme_bw() +
  theme(axis.title = element_text(color = "black", face = "bold"),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 9,
                                  color = "white",
                                  face = "bold",
                                  margin = margin(0.5, 0, 0.5, 0, "mm")),
        strip.background = element_rect(fill = "#0055a4",
                                        color = NA))

png(filename = 
      here::here(
        "analysis", 
        paste0("forecast_summer_bt_", prediction_month, "_", current_year, ".png")
      ),
    width = 120, 
    height = 120, 
    units = "mm",
    res = 300)
print(cowplot::plot_grid(p1, p2, nrow = 2, align = "hv"))
dev.off()

