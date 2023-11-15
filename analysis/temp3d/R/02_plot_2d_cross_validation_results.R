library(coldpool)
library(gapctd)
library(navmaps)
library(lubridate)

# Calculate root-mean square error, mean absolute error, and bias
cv_2d_table_plot <- function(region, 
                             model_order = c("No covariates", 
                                             "Linear depth", 
                                             "Quadratic depth", 
                                             "Log depth"),
                             write_output = TRUE) {
  
  cv_files <- list.files(here::here("output", region), full.names = TRUE)
  
  cv_dat <- data.frame()
  
  for(ii in 1:length(cv_files)) {
    
    dat <- readRDS(file = cv_files[ii])
    
    cv_dat <- dplyr::bind_rows(cv_dat,   
                               do.call(rbind, dat) |>
                                 dplyr::select(-DEPTH, -START_TIME, -LOG_GEAR_DEPTH) |>
                                 tidyr::pivot_longer(cols = c(ste, idw, exp, cir, gau, sph, mat, bes, nn, idw)))
      
    
  }
  
  output <- cv_dat |>
    dplyr::mutate(MODEL = ifelse(name %in% c("nn", "idw"), "None", MODEL)) |>
    dplyr::group_by(MODEL, name) |>
    dplyr::summarise(RMSE = sqrt(mean((value-TEMPERATURE)^2)),
                     MAE = mean(abs(value-TEMPERATURE)),
                     BIAS = 10^mean(abs(log10(value+2)-log10(TEMPERATURE+2)))) |>
    as.data.frame() |>
    
    dplyr::arrange(RMSE)
  
  cv_dat <- dplyr::mutate(cv_dat, MODEL = factor(MODEL, levels = model_order))
  
  rmse_plot <- ggplot() +
    geom_violin(data = cv_dat,
                mapping = aes(x = MODEL, 
                              y = sqrt((TEMPERATURE-value)^2))) +
    geom_violin(data = cv_dat,
                mapping = aes(x = MODEL, 
                              y = sqrt((TEMPERATURE-value)^2)),
                draw_quantiles = c(0.25, 0.75), linetype = 2, fill = NA) +
    geom_violin(data = cv_dat,
                mapping = aes(x = MODEL, 
                              y = sqrt((TEMPERATURE-value)^2)),
                draw_quantiles = c(0.5), fill = NA) +
    geom_point(data = cv_dat |>
                 dplyr::mutate(residual = (TEMPERATURE-value)^2) |>
                 dplyr::group_by(MODEL) |>
                 dplyr::summarise(mean_residual = sqrt(mean(residual))),
               mapping = aes(x = MODEL, 
                             y = mean_residual)) +
    scale_y_continuous(name = "RMSE") +
    scale_x_discrete(name = "Model") +
    theme_bw()
  
  if(write_output) {
    ragg::agg_png(filename = here::here("plots", "2d_cv_rmse_violin_", region, ".png"),
                  width = 120, height = 120, units = "mm", res = 300)
    print(rmse_plot)
    dev.off()
    
    write.csv(output, file = here::here("plots", paste0("2d_cv_rmse_table_", region, ".csv")), row.names = FALSE)
  }
  
  return(list(error_table = output,
              rmse_plot = rmse_plot))
}

# ebs_dat <- cv_2d_table_plot(region = "EBS", write_output = FALSE)
dat <- cv_2d_table_plot(region = "AI", write_output = FALSE)
# goa_dat <- cv_2d_table_plot(region = "GOA", write_output = FALSE)

best_variogram_model <- dat$error_table$name[which.min(dat$error_table$RMSE)]

region <- "AI"

# UTM zones based on the most frequent zone among survey samples.
crs_by_region <- data.frame(region = c("AI", "GOA", "EBS"),
                            utmcrs = c("EPSG:32660", "EPSG:32605", "EPSG:32602"),
                            aeacrs = "EPSG:3338")

profile_dat <- readRDS(here::here("data", region, paste0("profile_data_", region, ".rds")))

if(region == "EBS") {
  slope_dat <- readRDS(here::here("data", "slope", paste0("profile_data_slope.rds")))
  nbs_dat <- readRDS(here::here("data", "nbs", paste0("profile_data_nbs.rds")))
  
  profile_dat$profile <- dplyr::bind_rows(profile_dat$profile, slope_dat$profile, nbs_dat$profile)
  profile_dat$haul <- dplyr::bind_rows(profile_dat$haul, slope_dat$haul, nbs_dat$haul)
}

profile_dat$haul$YEAR <- lubridate::year(profile_dat$haul$START_TIME)

unique_years <- sort(unique(profile_dat$haul$YEAR))

for(ii in 1:length(unique_years)) {
  
  sel_haul <- dplyr::filter(profile_dat$haul, 
                            YEAR == unique_years[ii], 
                            !is.na(GEAR_DEPTH))
  sel_profile <- dplyr::filter(profile_dat$profile, 
                               HAUL_ID %in% unique(sel_haul$HAUL_ID),
                               !is.na(LONGITUDE),
                               !is.na(LATITUDE)) |>
    dplyr::inner_join(dplyr::select(sel_haul, HAUL_ID, BOTTOM_DEPTH, GEAR_DEPTH, YEAR)) |>
    dplyr::mutate(LOG_GEAR_DEPTH = log(GEAR_DEPTH))
  
  anis <- coldpool::estimate_anisotropy(x = dplyr::filter(sel_profile, CAST == "bottom"),
                                variable_name = "TEMPERATURE",
                                latitude_name = "LATITUDE",
                                longitude_name = "LONGITUDE",
                                input_crs = "WGS84",
                                interpolation_crs = crs_by_region$utmcrs[crs_by_region$region == region],
                                nm = Inf,
                                vgm_width = NULL,
                                variogram_model = best_variogram_model,
                                kriging_formula = variable_name ~ 1)
  
}
