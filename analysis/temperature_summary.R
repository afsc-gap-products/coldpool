library(coldpool)

# Consecutive years with above and below average temperatures
dplyr::mutate(cold_pool_index, 
              diff_bt = MEAN_GEAR_TEMPERATURE - mean(MEAN_GEAR_TEMPERATURE),
              diff_sst = MEAN_SURFACE_TEMPERATURE - mean(MEAN_SURFACE_TEMPERATURE),
              diff_area = AREA_LTE2_KM2 - mean(AREA_LTE2_KM2)) |> 
  dplyr::mutate(sign_area = sign(diff_area),
                sign_bt = sign(diff_bt),
                sign_sst = sign(diff_sst),
                z_bt = diff_bt/sd(MEAN_GEAR_TEMPERATURE),
                z_sst = diff_sst/sd(MEAN_SURFACE_TEMPERATURE),
                z_cpa = diff_area/sd(AREA_LTE2_KM2),
                group = YEAR < 2020) |>
  dplyr::inner_join(data.frame(sign_area = c(-1,1),
                               symbol_area = c("-","+"))) |>
  dplyr::inner_join(data.frame(sign_bt = c(-1,1),
                               symbol_bt = c("-","+"))) |>
  dplyr::inner_join(data.frame(sign_sst = c(-1, 1),
                               symbol_sst = c("-","+")))


mean(cold_pool_index$MEAN_GEAR_TEMPERATURE[cold_pool_index$YEAR <= 2010])
mean(cold_pool_index$MEAN_GEAR_TEMPERATURE[cold_pool_index$YEAR > 2010])

mean(cold_pool_index$AREA_LTE2_KM2)
range(cold_pool_index$AREA_LTE2_KM2)

mean(cold_pool_index$AREA_LTE2_KM2)/493850
range(cold_pool_index$AREA_LTE2_KM2)/493850

mean(cold_pool_index$AREA_LTE0_KM2)
range(cold_pool_index$AREA_LTE0_KM2)

mean(cold_pool_index$AREA_LTE0_KM2)/493850
range(cold_pool_index$AREA_LTE0_KM2)/493850

Hmisc::rcorr(cold_pool_index |>
               dplyr::select(AREA_LTE2_KM2, AREA_LTE1_KM2, AREA_LTE0_KM2, AREA_LTEMINUS1_KM2,MEAN_GEAR_TEMPERATURE, MEAN_SURFACE_TEMPERATURE) |>
               as.matrix(),
             type = "pearson")


cor.test(coldpool::cold_pool_index$AREA_LTE0_KM2, 
         coldpool::cold_pool_index$AREA_LTE2_KM2)

cor.test(coldpool::cold_pool_index$MEAN_GEAR_TEMPERATURE, 
         coldpool::cold_pool_index$MEAN_SURFACE_TEMPERATURE)

cor.test(coldpool::cold_pool_index$AREA_LTE2_KM2, 
         coldpool::cold_pool_index$MEAN_SURFACE_TEMPERATURE)

cor.test(coldpool::cold_pool_index$AREA_LTE2_KM2, 
         coldpool::cold_pool_index$MEAN_GEAR_TEMPERATURE)

cor.test(coldpool::nbs_mean_temperature$MEAN_GEAR_TEMPERATURE,
         coldpool::nbs_mean_temperature$MEAN_SURFACE_TEMPERATURE)

plot(coldpool::nbs_ebs_bottom_temperature-mean(coldpool::nbs_ebs_bottom_temperature, na.rm = TRUE))



