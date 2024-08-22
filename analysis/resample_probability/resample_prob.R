
library(coldpool)
library(akgfmaps)
library(mgcv)

channel <- coldpool::get_connected(schema = "AFSC")

index_hauls <- RODBC::sqlQuery(channel = channel,
                query = "select * from racebase.haul 
                where haul_type in (3, 17) 
                and region = 'BS'
                and cruise > 199900") |>
  dplyr::filter(STATIONID %in% akgfmaps::get_survey_stations(select.region = "sebs")) |>
  dplyr::mutate(YEAR = floor(CRUISE/100))

bristol_bay_stns <- dplyr::bind_rows(
  index_hauls[grepl(pattern = 16, x = index_hauls$STATIONID), ],
  index_hauls[grepl(pattern = 15, x = index_hauls$STATIONID), ]
) |>
  dplyr::group_by(YEAR) |>
  dplyr::summarise(MEAN_CORE_BT = mean(GEAR_TEMPERATURE, na.rm = TRUE))

resample_years <- index_hauls |> 
  dplyr::group_by(YEAR, HAUL_TYPE) |>
  dplyr::summarise(n = n()) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(id_cols = "YEAR",
                     names_from = "HAUL_TYPE",
                     values_from = n,
                     values_fill = 0) |>
  dplyr::mutate(RESAMPLE = `17` > 0) |>
  dplyr::inner_join(coldpool::cold_pool_index) |>
  dplyr::inner_join(bristol_bay_stns)


mod_resample1 <- mgcv::gam(formula = RESAMPLE ~ s(AREA_LTE2_KM2, bs = "tp", k = 5), 
                           family = binomial(), 
                           data = resample_years)

mod_resample2 <- mgcv::gam(formula = RESAMPLE ~ s(MEAN_CORE_BT, bs = "tp", k = 5), 
                           family = binomial(), 
                           data = resample_years)

mod_summary1 <- summary(mod_resample1)
mod_summary1$dev.expl
mod_summary2 <- summary(mod_resample2)
mod_summary2$dev.expl

gam.check(mod_resample1)
gam.check(mod_resample2)

resample_years$fit_1 <- predict(mod_resample1, type = "response")
resample_years$fit_2 <- predict(mod_resample2, type = "response")

fit1 <- data.frame(AREA_LTE2_KM2 = 
                     seq(min(resample_years$AREA_LTE2_KM2), 
                         max(resample_years$AREA_LTE2_KM2),
                         by = 5000))

fit1$mean <- predict(mod_resample1, 
                     newdata = fit1)

fit1$se <- predict(mod_resample1, 
                       newdata = fit1, 
                       se.fit = TRUE)$se.fit

fit2 <- data.frame(MEAN_CORE_BT = 
                     seq(min(resample_years$MEAN_CORE_BT), 
                         max(resample_years$MEAN_CORE_BT),
                         by = 0.1))

fit2$mean <- predict(mod_resample2, 
                     newdata = fit2)

fit2$se <- predict(mod_resample2, 
                   newdata = fit2, 
                   se.fit = TRUE)$se.fit

inv_logit <- function(x) {
  exp(x)/(1+exp(x))
}


ragg::agg_png(here::here("analysis", "resample_probability", "fit_cpa.png"), width = 6, height = 4, units = "in", res = 300)
print(
ggplot() +
  geom_ribbon(data = fit1, 
              mapping = aes(x = AREA_LTE2_KM2,
                            ymin = inv_logit(mean-se),
                            ymax = inv_logit(mean+se)),
              alpha = 0.3) +
  geom_path(data = fit1, 
            mapping = aes(x = AREA_LTE2_KM2,
                          y = inv_logit(mean))) +
  geom_point(data = resample_years,
             mapping = aes(x = AREA_LTE2_KM2, 
                           y = as.numeric(RESAMPLE), 
                           color = RESAMPLE)) +
  scale_color_manual(name = "Resampled?", 
                     values = c("TRUE" = "#E69F00", "FALSE" = "black")) + 
  scale_x_continuous(name = expression('Cold Pool Area ('*km^2*')')) +
  scale_y_continuous(name = "Resample Probability (fit)") +
  ggtitle(label = paste0("Predictor = Cold pool area (km^2)\nDeviance explained: ", 
                         round(mod_summary1$dev.expl*100, 1), "%")) +
  theme_bw()
)
dev.off()


ragg::agg_png(here::here("analysis", "resample_probability", "fit_col1516_temp.png"), width = 6, height = 4, units = "in", res = 300)
print(
ggplot() +
  geom_ribbon(data = fit2, 
              mapping = aes(x = MEAN_CORE_BT,
                            ymin = inv_logit(mean-se),
                            ymax = inv_logit(mean+se)),
              alpha = 0.3) +
  geom_path(data = fit2, 
            mapping = aes(x = MEAN_CORE_BT,
                          y = inv_logit(mean))) +
  geom_point(data = resample_years,
             mapping = aes(x = MEAN_CORE_BT, 
                           y = as.numeric(RESAMPLE), 
                           color = RESAMPLE)) +
  scale_color_manual(name = "Resampled?", 
                     values = c("TRUE" = "#E69F00", "FALSE" = "black")) + 
  scale_x_continuous(name = expression('Mean bottom temperature in columns 15-16 ('*degree*C*')')) +
  scale_y_continuous(name = "Resample Probability (fit)") +
  ggtitle(label = paste0("Predictor = Mean bottom temperature for stations in columns 15-16\nDeviance explained: ", 
                         round(mod_summary2$dev.expl*100, 1), "%")) +
  theme_bw()
)
dev.off()


ragg::agg_png(here::here("analysis", "resample_probability", "timeseries_cpa.png"), width = 6, height = 4, units = "in", res = 300)
print(
ggplot() +
  geom_path(data = resample_years,
             mapping = aes(x = YEAR, 
                           y = fit_1)) +
  geom_point(data = resample_years,
             mapping = aes(x = YEAR, 
                           y = fit_1,
                           color = RESAMPLE)) +
  scale_color_manual(name = "Resampled?", 
                     values = c("TRUE" = "#E69F00", "FALSE" = "black")) + 
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Resample Probability (fit)") +
  ggtitle(label = paste0("Predictor = Cold pool area (km^2)\nDeviance explained: ", 
                         round(mod_summary1$dev.expl*100, 1), "%")) +
  theme_bw()
)
dev.off()


ragg::agg_png(here::here("analysis", "resample_probability", "timeseries_col1516_temp.png"), width = 6, height = 4, units = "in", res = 300)
print(
ggplot() +
  geom_path(data = resample_years,
            mapping = aes(x = YEAR, 
                          y = fit_2)) +
  geom_point(data = resample_years,
             mapping = aes(x = YEAR, 
                           y = fit_2,
                           color = RESAMPLE)) +
  scale_color_manual(name = "Resampled?", 
                     values = c("TRUE" = "#E69F00", "FALSE" = "black")) + 
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "Resample Probability (fit)") +
  ggtitle(label = paste0("Predictor = Mean bottom temperature for stations in columns 15-16\nDeviance explained: ", 
                         round(mod_summary2$dev.expl*100, 1), "%")) +
  theme_bw()
)
dev.off()

