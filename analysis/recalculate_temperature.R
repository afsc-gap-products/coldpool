library(ggplot2)
library(dplyr)
library(gapctd)
library(plotly)

channel <- gapctd::get_connected(schema = "AFSC")

# Get vessel/cruise haul data
vc_df <- RODBC::sqlQuery(channel = channel, query = paste0("select vessel, cruise from racebase.haul where region = 'AI' and cruise = 202201")) |>
  unique() |>
  dplyr::filter(CRUISE %% 10 == 1)

# Retrieve temperature methods 
haul_temp_methods <- RODBC::sqlQuery(channel = channel,
                                     query = "select a.cruise_id, a.haul, a.haul_id, a.surface_temperature_method, a.surface_temperature, a.gear_temperature, a.gear_temperature_method from race_data.hauls a") |>
  dplyr::inner_join(RODBC::sqlQuery(channel = channel, 
                                    query = "select temperature_method_id, name from race_data.temperature_methods") |>
                      dplyr::mutate(GEAR_TEMPERATURE_METHOD = TEMPERATURE_METHOD_ID,
                                    GEAR_TEMPERATURE_METHOD_NAME = NAME) |> 
                      dplyr::select(-TEMPERATURE_METHOD_ID,
                                    -NAME)) |>
  dplyr::inner_join(RODBC::sqlQuery(channel = channel, 
                                    query = "select temperature_method_id, name from race_data.temperature_methods") |>
                      dplyr::mutate(SURFACE_TEMPERATURE_METHOD = TEMPERATURE_METHOD_ID,
                                    SURFACE_TEMPERATURE_METHOD_NAME = NAME) |> 
                      dplyr::select(-TEMPERATURE_METHOD_ID,
                                    -NAME)) |> 
  dplyr::inner_join(RODBC::sqlQuery(channel = channel, query = "select survey_definition_id, survey_name from race_data.survey_definitions where (survey_definition_id in (47, 52, 98, 143))") |>
                      dplyr::inner_join(RODBC::sqlQuery(channel = channel, query = "select survey_id, survey_definition_id from race_data.surveys")) |>
                      dplyr::inner_join(RODBC::sqlQuery(channel = channel, query = "select cruise_id, vessel_id vessel, survey_id, cruise from race_data.cruises")))

aibt <- try(RODBC::sqlQuery(channel = channel, query = paste("select a.vessel_id vessel, a.cruise cruise, b.haul haul, 
			d.edit_date_time date_time, d.edit_depth depth, d.edit_temperature temperature 
			from race_data.cruises a, race_data.hauls b, race_data.bathythermic_headers c, 
			race_data.bathythermics d, race_data.datum_codes e, race_data.surveys f, 
			race_data.survey_definitions g where g.survey_definition_id = f.survey_definition_id 
			and year > 2021 and g.survey_name = 'Aleutian Islands Bottom Trawl Survey' and f.survey_id = a.survey_id 
			and b.cruise_id = a.cruise_id and c.haul_id = b.haul_id and d.bathythermic_header_id = c.bathythermic_header_id and e.datum_code = d.datum_code and d.edit_depth > -0.01
			and e.use_in_analysis = 'Y' order by vessel, haul", sep = "")) |> 
               dplyr::mutate(DATE_TIME = lubridate::force_tz(DATE_TIME, tzone = "America/Anchorage")), silent = TRUE)

ref_depth <- 1

## Upcasts ----

upcast_out_df <- NULL

for(kk in 1:nrow(vc_df)) {
  
  print(paste0(kk, " out of ", nrow(vc_df)))
  
  v_sel <- vc_df$VESSEL[kk]
  c_sel <- vc_df$CRUISE[kk]
  y_sel <- floor(c_sel/100)
  
  haul_events <- RODBC::sqlQuery(channel = channel, query = paste0("select a.event_type_id, a.date_time, b.haul, c.vessel_id vessel, c.cruise from race_data.events a, race_data.hauls b, race_data.cruises c where a.haul_id = b.haul_id and b.cruise_id = c.cruise_id and c.vessel_id = ", v_sel, "and cruise = ", c_sel))
  
  
  haul_temp <- RODBC::sqlQuery(channel = channel, query = paste0("select vessel, cruise, haul from racebase.haul where region = 'AI' and vessel = ", v_sel, "and cruise = ", c_sel))
  
  haul_temp$RECALCULATE <- -99
  
  for(ii in 1:nrow(haul_temp)) {
    
    print(ii)
    
    obtime <- (haul_events |>
                 dplyr::filter(VESSEL == haul_temp$VESSEL[ii], CRUISE == haul_temp$CRUISE[ii], HAUL == haul_temp$HAUL[ii], EVENT_TYPE_ID == 7))
    
    if(!(nrow(obtime) >= 1)) {
      next
    }
    
    obtime <- obtime$DATE_TIME |> 
      lubridate::force_tz(tz = "UTC") |>
      lubridate::with_tz(tzone = "America/Anchorage")
    
    # DATE_TIME in order
    sel_haul <- aibt |>
      dplyr::filter(VESSEL == haul_temp$VESSEL[ii], CRUISE == haul_temp$CRUISE[ii], HAUL == haul_temp$HAUL[ii], DATE_TIME > obtime, DEPTH <= (ref_depth + 5)) |>
      dplyr::arrange(DATE_TIME)
    
    haul_temp$RECALCULATE[ii] <- gapctd::calc_fixed_depth_var(depth = sel_haul$DEPTH, var = sel_haul$TEMPERATURE, ref_depth = ref_depth)
  }
  
  
  haul_temp <- haul_temp |>
    dplyr::inner_join(haul_temp_methods, 
                      by = c("VESSEL", "CRUISE", "HAUL"))
  
  if(is.null(upcast_out_df)) {
    upcast_out_df <- haul_temp
  } else {
    upcast_out_df <- dplyr::bind_rows(upcast_out_df, haul_temp)
  }
  
}

upcast_out_df$RECALCULATE[upcast_out_df$RECALCULATE == -99] <- NA
upcast_out_df$diff <- upcast_out_df$SURFACE_TEMPERATURE - round(upcast_out_df$RECALCULATE, 3)

saveRDS(upcast_out_df, here::here("output", "bt_algorithms_upcast.rds"))

upcast_high_diff <- upcast_out_df |>
  dplyr::filter(abs(diff) > 0.15) |>
  dplyr::arrange(CRUISE, VESSEL, HAUL)

for(jj in 1:nrow(upcast_high_diff)) {
  
  sel_diff <- aibt |>
    dplyr::filter(VESSEL == upcast_high_diff$VESSEL[jj], 
                  CRUISE == upcast_high_diff$CRUISE[jj], 
                  HAUL == upcast_high_diff$HAUL[jj],
                  DEPTH <= 5) |>
    dplyr::mutate(DEPTH = DEPTH * -1)
  
  png(here::here("plots", "surf_temp_diff", paste0("st_diff_upcast_", upcast_high_diff$CRUISE[jj], "_", upcast_high_diff$VESSEL[jj], "_", upcast_high_diff$HAUL[jj], ".png")), 
      width = 7, 
      height = 7, 
      units = "in", 
      res = 300)
  print(ggplot() + 
          geom_point(data = sel_diff |>
                       tidyr::pivot_longer(cols = c("DEPTH", "TEMPERATURE")) |>
                       dplyr::arrange(DATE_TIME),
                     aes(x = DATE_TIME, 
                         y = value, 
                         color = name)) +
          geom_hline(data = upcast_high_diff[jj,] |>
                       tidyr::pivot_longer(cols = c("SURFACE_TEMPERATURE", "RECALCULATE")) |>
                       dplyr::rename(type = name) |>
                       dplyr::mutate(name = "TEMPERATURE"),
                     mapping = aes(yintercept = value, 
                                   lty = type),
                     size = rel(1.2)) +
          facet_wrap(~name, scales = "free_y", nrow = 2) +
          ggthemes::scale_color_tableau() +
          ggtitle(paste0("Cruise: ", upcast_high_diff[jj,]$CRUISE, ", Vessel: ", upcast_high_diff[jj,]$VESSEL, ", Haul: ", upcast_high_diff[jj,]$HAUL, ", Method: ", upcast_high_diff[jj,]$SURFACE_TEMPERATURE_METHOD)) +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5)))
  dev.off()
}


## Downcasts ----

downcast_out_df <- NULL

for(kk in 1:nrow(vc_df)) {
  print(paste0(kk, " out of ", nrow(vc_df)))
  v_sel <- vc_df$VESSEL[kk]
  c_sel <- vc_df$CRUISE[kk]
  y_sel <- floor(c_sel/100)
  
  haul_events <- RODBC::sqlQuery(channel = channel, query = paste0("select a.event_type_id, a.date_time, b.haul, c.vessel_id vessel, c.cruise from race_data.events a, race_data.hauls b, race_data.cruises c where a.haul_id = b.haul_id and b.cruise_id = c.cruise_id and c.vessel_id = ", v_sel, "and cruise = ", c_sel))
  
  
  haul_temp <- RODBC::sqlQuery(channel = channel, query = paste0("select vessel, cruise, haul from racebase.haul where region = 'BS' and vessel = ", v_sel, "and cruise = ", c_sel))
  
  haul_temp$RECALCULATE <- -99
  
  for(ii in 1:nrow(haul_temp)) {
    print(ii)
    obtime <- (haul_events |>
                 dplyr::filter(VESSEL == haul_temp$VESSEL[ii], CRUISE == haul_temp$CRUISE[ii], HAUL == haul_temp$HAUL[ii], EVENT_TYPE_ID == 3))
    
    if(!(nrow(obtime) >= 1)) {
      next
    }
    
    obtime <- obtime$DATE_TIME |> 
      lubridate::force_tz(tz = "UTC") |>
      lubridate::with_tz(tzone = "America/Anchorage")
    
    # DATE_TIME in order
    sel_haul <- aibt |>
      dplyr::filter(VESSEL == haul_temp$VESSEL[ii], CRUISE == haul_temp$CRUISE[ii], HAUL == haul_temp$HAUL[ii], DATE_TIME < obtime, DEPTH <= (ref_depth + 5)) |>
      dplyr::arrange(desc(DATE_TIME))
    
    haul_temp$RECALCULATE[ii] <- gapctd::calc_fixed_depth_var(depth = sel_haul$DEPTH, var = sel_haul$TEMPERATURE, ref_depth = ref_depth )
  }
  
  
  haul_temp <- haul_temp |>
    dplyr::inner_join(haul_temp_methods, 
                      by = c("VESSEL", "CRUISE", "HAUL"))
  # haul_temp$diff <- haul_temp$SURFACE_TEMPERATURE - round(haul_temp$RECALCULATE, 3)
  
  if(is.null(downcast_out_df)) {
    downcast_out_df <- haul_temp
  } else {
    downcast_out_df <- dplyr::bind_rows(downcast_out_df, haul_temp)
  }
}

downcast_out_df$RECALCULATE[downcast_out_df$RECALCULATE == -99] <- NA
downcast_out_df$diff <- downcast_out_df$SURFACE_TEMPERATURE - round(downcast_out_df$RECALCULATE, 3)

saveRDS(downcast_out_df, here::here("output", "bt_algorithms_downcast.rds"))

ggplot() +
  geom_point(data = downcast_out_df, 
             aes(x = floor(CRUISE/100), 
                 y = diff,
                 group = CRUISE)) +
  theme_minimal()

ggplot() +
  geom_point(data = upcast_out_df, 
             aes(x = floor(CRUISE/100), 
                 y = diff,
                 group = CRUISE)) +
  theme_minimal()

downcast_high_diff <- downcast_out_df |>
  dplyr::filter(abs(diff) > 0.15) |>
  dplyr::arrange(CRUISE, VESSEL, HAUL)

for(jj in 1:nrow(downcast_high_diff)) {
  
  sel_diff <- aibt |>
    dplyr::filter(VESSEL == downcast_high_diff$VESSEL[jj], 
                  CRUISE == downcast_high_diff$CRUISE[jj], 
                  HAUL == downcast_high_diff$HAUL[jj],
                  DEPTH <= (ref_depth + 5)) |>
    dplyr::mutate(DEPTH = DEPTH * -1)
  
  png(here::here("plots", "surf_temp_diff", paste0("st_diff_downcast_", downcast_high_diff$CRUISE[jj], "_", downcast_high_diff$VESSEL[jj], "_", downcast_high_diff$HAUL[jj], ".png")), width = 7, height = 7, units = "in", res = 300)
  print(ggplot() + 
          geom_point(data = sel_diff |>
                       tidyr::pivot_longer(cols = c("DEPTH", "TEMPERATURE")) |>
                       dplyr::arrange(DATE_TIME),
                     aes(x = DATE_TIME, 
                         y = value, 
                         color = name)) +
          geom_hline(data = downcast_high_diff[jj,] |>
                       tidyr::pivot_longer(cols = c("SURFACE_TEMPERATURE", "RECALCULATE")) |>
                       dplyr::rename(type = name) |>
                       dplyr::mutate(name = "TEMPERATURE"),
                     mapping = aes(yintercept = value, 
                                   lty = type),
                     size = rel(1.2)) +
          facet_wrap(~name, scales = "free_y", nrow = 2) +
          ggthemes::scale_color_tableau() +
          ggtitle(paste0("Cruise: ", downcast_high_diff[jj,]$CRUISE, ", Vessel: ", downcast_high_diff[jj,]$VESSEL, ", Haul: ", downcast_high_diff[jj,]$HAUL, ", Method: ", downcast_high_diff[jj,]$SURFACE_TEMPERATURE_METHOD)) +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5)))
  dev.off()
}

# Algorithm comparison

readRDS(here::here("output", "bt_algorithms_upcast.rds")) |>
  dplyr::mutate(cast = "upcast") |>
  dplyr::filter(!is.na(diff)) |>
  dplyr::group_by(CRUISE, cast) |>
  dplyr::summarise(min_diff = min(diff, na.rm = TRUE),
                   max_diff = max(diff, na.rm = TRUE),
                   mean_diff = mean(diff, na.rm = TRUE),
                   mean_abs_diff = mean(abs(diff), na.rm = TRUE),
                   sd_diff = sd(diff, na.rm = TRUE),
                   n = n())

table(upcast_out_df$SURFACE_TEMPERATURE_METHOD_NAME[abs(upcast_out_df$diff) > 0],
      upcast_out_df$CRUISE[abs(upcast_out_df$diff) > 0])

table(upcast_out_df$CRUISE[abs(upcast_out_df$diff) > 0],
      upcast_out_df$FLAG[abs(upcast_out_df$diff) > 0])


readRDS(here::here("output", "bt_algorithms_downcast.rds")) |>
  dplyr::mutate(cast = "downcast") |>
  dplyr::filter(!is.na(diff)) |>
  dplyr::group_by(CRUISE, cast) |>
  dplyr::summarise(min_diff = min(diff, na.rm = TRUE),
                   max_diff = max(diff, na.rm = TRUE),
                   mean_diff = mean(diff, na.rm = TRUE),
                   mean_abs_diff = mean(abs(diff), na.rm = TRUE),
                   sd_diff = sd(diff, na.rm = TRUE),
                   n = n())

table(downcast_out_df$CRUISE[abs(downcast_out_df$diff) > 0])
table(upcast_out_df$CRUISE[abs(upcast_out_df$diff) > 0])

table(downcast_out_df$SURFACE_TEMPERATURE_METHOD_NAME[abs(downcast_out_df$diff) > 0],
      downcast_out_df$CRUISE[abs(downcast_out_df$diff) > 0])

table(downcast_out_df$CRUISE[abs(downcast_out_df$diff) > 0],
      downcast_out_df$FLAG[abs(downcast_out_df$diff) > 0])

# Density plots ----

bt_algorithm_df <- dplyr::bind_rows(
  readRDS(here::here("output", "bt_algorithms_upcast.rds")) |>
    dplyr::mutate(cast = "upcast"),
  readRDS(here::here("output", "bt_algorithms_downcast.rds")) |>
    dplyr::mutate(cast = "downcast")) |> 
  tidyr::pivot_wider(id_cols = c("VESSEL", "CRUISE", "HAUL"), names_from = "cast", values_from = c("RECALCULATE")) |>
  dplyr::mutate(diff = upcast - downcast) |>
  dplyr::group_by(CRUISE, VESSEL) |>
  dplyr::summarise(mean_up_minus_down = mean(diff, na.rm = TRUE))

print(bt_algorithm_df, n = 40)


png(file = here::here("plots", "surf_temp_diff", "0_algorithm_vs_surface"))
print(
  ggplot() +
    geom_histogram(data = dplyr::bind_rows(
      readRDS(here::here("output", "bt_algorithms_upcast.rds")) |>
        dplyr::mutate(cast = "upcast"),
      readRDS(here::here("output", "bt_algorithms_downcast.rds")) |>
        dplyr::mutate(cast = "downcast")), 
      aes(x = diff, color = cast, fill = cast), 
      alpha = 0.3,
      position = "identity", bins = 20) +
    facet_wrap(~floor(CRUISE/100)) +
    scale_x_continuous(limits = c(-3,3), name = expression(T["surface,RACEBASE"]-T["surface,algorithm"])) +
    scale_y_continuous(limits = c(0, 460), expand = c(0,0)) +
    ggthemes::scale_color_tableau() +
    ggthemes::scale_fill_tableau() +
    theme_bw()
)
dev.off()