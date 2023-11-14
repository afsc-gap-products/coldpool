library(coldpool)

region <- "EBS"

cv_files <- list.files(here::here("analysis", "temp3d", "output", region), full.names = TRUE)

cv_dat <- data.frame()

for(ii in 1:length(cv_files)) {
  
  dat <- readRDS(file = cv_files[ii])
  
  cv_dat <- dplyr::bind_rows(cv_dat,   
                             do.call(rbind, dat) |>
                               dplyr::select(-DEPTH, -START_TIME, -LOG_GEAR_DEPTH) |>
                               tidyr::pivot_longer(cols = c(ste, idw, exp, cir, gau, sph, mat, bes, nn, idw)))
  
  
}

cv_dat |>
  dplyr::mutate(MODEL = ifelse(name %in% c("nn", "idw"), "None", MODEL)) |>
  dplyr::group_by(MODEL, name) |>
  dplyr::summarise(RMSE = sqrt(mean((value-TEMPERATURE)^2)),
                   MAE = mean(abs(value-TEMPERATURE)),
                   BIAS = 10^mean(abs(log10(value+2)-log10(TEMPERATURE+2)))
                   ) |>
  as.data.frame() |>
  dplyr::arrange(BIAS)


loocv_long$TEMPERATURE-loocv_long$value

ggplot() +
  geom_boxplot(data = cv_dat,
              mapping = aes(x = MODEL, 
                            y = sqrt((TEMPERATURE-value)^2))) +
  geom_point(data = cv_dat |>
               dplyr::mutate(sq_error = (TEMPERATURE-value)^2) |>
               dplyr::group_by(MODEL) |>
               dplyr::summarise(rmse = sqrt(mean(sq_error))),
             mapping = aes(x = MODEL, y = rmse)) +
  scale_y_continuous(name = "Square Error")
