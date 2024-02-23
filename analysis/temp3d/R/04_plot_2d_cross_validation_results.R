library(coldpool)
library(gapctd)
library(navmaps)
library(lubridate)

# Calculate root-mean square error, mean absolute error, and bias
cv_2d_table_plot <- function(region, 
                             model_order = c("NN",
                                             "IDW",
                                             "OK", 
                                             "RK (Linear depth)", 
                                             "RK (Quadratic depth)", 
                                             "RK (Log depth)")) {
    
  dat <- readRDS(file = list.files(here::here("output", region), 
                                     full.names = TRUE, pattern = "2d_interp_cv_temperature")) |>
                                 dplyr::select(-DEPTH) |>
                                 tidyr::pivot_longer(cols = c(ste, idw, exp, cir, gau, sph, mat, bes, nn, idw)) |>
    dplyr::filter(!is.na(value))
  
  bias_offset <- 0
  if(min(c(dat$value, dat$TEMPERATURE)) <= 0) {
    bias_offset <- abs(min(c(dat$value, dat$TEMPERATURE))) + 1e-5
  }
      
  output <- dat |>
    dplyr::group_by(MODEL, name) |>
    dplyr::summarise(RMSE = sqrt(mean((value-TEMPERATURE)^2)),
                     MAE = mean(abs(value-TEMPERATURE)),
                     BIAS = 10^mean(abs(log10(value+bias_offset)-log10(TEMPERATURE+bias_offset)))) |>
    as.data.frame() |>
    dplyr::arrange(RMSE)
  
  dat <-  dat |>
    dplyr::mutate(MODEL = factor(MODEL, levels = model_order))

  rmse_plot <- ggplot() +
    geom_violin(data = dat,
                mapping = aes(x = MODEL, 
                              y = sqrt((TEMPERATURE-value)^2))) +
    geom_violin(data = dat,
                mapping = aes(x = MODEL, 
                              y = sqrt((TEMPERATURE-value)^2)),
                draw_quantiles = c(0.25, 0.75), linetype = 2, fill = NA) +
    geom_violin(data = dat,
                mapping = aes(x = MODEL, 
                              y = sqrt((TEMPERATURE-value)^2)),
                draw_quantiles = c(0.5), fill = NA) +
    geom_point(data = dat |>
                 dplyr::mutate(residual = (TEMPERATURE-value)^2) |>
                 dplyr::group_by(MODEL) |>
                 dplyr::summarise(mean_residual = sqrt(mean(residual))),
               mapping = aes(x = MODEL, 
                             y = mean_residual)) +
    scale_y_continuous(name = "MSE") +
    scale_x_discrete(name = "Model") +
    theme_bw()
  
    ragg::agg_png(filename = here::here("plots", "2d_cv_rmse_violin_", region, ".png"),
                  width = 120, height = 120, units = "mm", res = 300)
    print(rmse_plot)
    dev.off()
    
    write.csv(output, file = here::here("plots", paste0("2d_cv_rmse_table_", region, ".csv")), row.names = FALSE)
  
  return(list(error_table = output,
              rmse_plot = rmse_plot))
    
}

ebs_dat <- cv_2d_table_plot(region = "EBS")
ai_dat <- cv_2d_table_plot(region = "AI")
goa_dat <- cv_2d_table_plot(region = "GOA")
