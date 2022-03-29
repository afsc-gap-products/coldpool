#' Make data product figures for TM
#' 
#' Make data product figures that are formatted like 2021 ESR figures.
#' 
#' @param fig_res Figure resolution
#' @noRd

make_tm_product_figs <- function(fig_res = 600) {
  
  # Setup ----
  
  if(!dir.exists("plots")) {dir.create("plots")}
  
  cpa_palette <- c("#21dae7", "#0071ff", "#0000e3", "#000040")
  max_year <- 1982+19
  min_year <- max_year - 19 # Default for 20 panel cold pool maps
  
  cpi_df <- coldpool:::cold_pool_index %>%
    dplyr::mutate(group = YEAR < 2020,
                  var = "Cold Pool Index",
                  cpi_rank = rank(AREA_LTE2_KM2))
  
  cpi_sd <- sd(cpi_df$AREA_LTE2_KM2/1000)
  cpi_mean <- mean(cpi_df$AREA_LTE2_KM2/1000)
  
  plot_cpi_timeseries <- ggplot(data = cpi_df,
                                mapping = aes(x = YEAR,
                                              y = AREA_LTE2_KM2/1000,
                                              group = group,
                                              label = cpi_rank)) +
    geom_point(size = rel(2),
               color = "#000040") +
    geom_line(color = "#000040") +
    geom_hline(yintercept = cpi_mean,
               linetype = 2,
               color = "#000040") +
    geom_hline(yintercept = cpi_mean + c(cpi_sd, -1*cpi_sd),
               linetype = 3,
               color = "#000040") +
    scale_y_continuous(name = expression(bold("Cold Pool Area"~("thousand"~km^2)))) +
    scale_x_continuous(name = "Year",
                       breaks = seq(1980,2020,4)) +
    facet_wrap(~var) +
    theme_bw() +
    theme(axis.title = element_text(color = "black", face = "bold"),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(size = 9,
                                    color = "white",
                                    face = "bold",
                                    margin = margin(0.5, 0, 0.5, 0, "mm")),
          strip.background = element_rect(fill = "#0055a4",
                                          color = NA))
  
  # tm_stacked_temperature.png ----
  
  
  sebs_layers <- akgfmaps::get_base_layers(select.region = "sebs",
                                           set.crs = coldpool:::ebs_proj_crs)
  
  area_df <- coldpool:::cold_pool_index %>%
    dplyr::mutate(lteminus1 = AREA_LTEMINUS1_KM2,
                  lte0 = AREA_LTE0_KM2 - AREA_LTEMINUS1_KM2,
                  lte1 = AREA_LTE1_KM2 - AREA_LTE0_KM2,
                  lte2 = AREA_LTE2_KM2 - AREA_LTE1_KM2) %>%
    dplyr::select(YEAR, lteminus1, lte0, lte1, lte2) %>%
    reshape2::melt(id.vars = "YEAR") %>%
    dplyr::mutate(variable = factor(variable, 
                                    levels = c( "lte2", "lte1", "lte0", "lteminus1"),
                                    labels = c("\u22642 \u00b0C", "\u22641 \u00b0C", "\u22640 \u00b0C", "\u2264-1 \u00b0C")),
                  proportion = value/sebs_layers$survey.area$AREA_KM2)
  
  stacked_area_plot <- ggplot() +
    geom_area(data = area_df %>%
                dplyr::filter(YEAR < 2020),
              mapping = aes(x = YEAR,
                            y = proportion,
                            fill = variable)) +
    geom_point(data = area_df %>%
                 dplyr::filter(YEAR == 2021),
               mapping = aes(x = YEAR,
                             y = proportion,
                             fill = variable),
               shape = 21,
               size = rel(3)) +
    scale_fill_manual(name = "Temperature", 
                      values = cpa_palette) +
    scale_y_continuous(name = "Proportion of EBS Shelf (Plus NW) Survey Area",
                       limits = c(0, 1),
                       expand = c(0, 0),
                       breaks = seq(0,1,0.1)) +
    scale_x_continuous(name = "Year", 
                       limits = c(1982, 2021.25),
                       expand = c(0, 0),
                       breaks = seq(1982,2021,4)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          axis.title = element_text(face = "bold"),
          axis.ticks = element_line(color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          panel.background = element_rect(color = "black", fill = NA),
          legend.position = c(0.2, 0.8),
          legend.title = element_blank())
  
  # SEBS: tm_coldpool_grid_1.png ----
  
  year_vec <- as.numeric(gsub("[^0-9.-]", "", names(coldpool:::ebs_bottom_temperature)))
  start_year <- which(year_vec == min_year)
  mid_year <- which(year_vec == max_year)
  end_year <- which(year_vec == 2021)
  
  coords <- raster::coordinates(coldpool:::ebs_bottom_temperature)
  
  for(i in start_year:mid_year) {
    sel_layer_df <- data.frame(x = coords[,1],
                               y = coords[,2],
                               temperature = coldpool:::ebs_bottom_temperature@data@values[,i])
    sel_layer_df <- sel_layer_df[!is.na(sel_layer_df$temperature),]
    sel_layer_df <- sel_layer_df %>%
      dplyr::filter(temperature <= 2)
    sel_layer_df$year <- year_vec[i]
    
    if(i == start_year) {
      bt_year_df <- sel_layer_df
    } else{
      bt_year_df <- dplyr::bind_rows(bt_year_df, sel_layer_df)
    }
  }
  
  bt_year_df$temp_disc <- cut(bt_year_df$temperature, 
                              breaks = c(-2, -1, 0, 1, 2))
  
  cpa_palette <- c("#21dae7", "#0071ff", "#0000e3", "#000040")
  
  panel_extent <- data.frame(y = c(52, 64),
                             x = c(-175, -156)) %>%
    akgfmaps::transform_data_frame_crs(out.crs = coldpool:::ebs_proj_crs)
  
  label_2020 <- data.frame(x = mean(panel_extent$x),
                           y = mean(panel_extent$y),
                           label = "No\nSurvey",
                           year = 2020) %>% 
    dplyr::filter(year <= max_year)
  
  cold_pool_cbar <- coldpool::legend_discrete_cbar(breaks = c(-Inf, -1, 0, 1, 2),
                                                   colors = rev(c("#21dae7", "#0071ff", "#0000e3", "#000040")),
                                                   legend_direction = "vertical",
                                                   font_size = 3.5,
                                                   width = 0.1,
                                                   expand_size.x = 0.3,
                                                   expand_size.y = 0.3,
                                                   expand.x = 0.2,
                                                   expand.y = 0.9,
                                                   spacing_scaling = 1,
                                                   text.hjust = 0,
                                                   font.family = "sans",
                                                   neat.labels = FALSE) + 
    annotate("text", 
             x = 1.1, 
             y = 2.05, 
             label =  expression(bold("Bottom\nTemperature"~(degree*C))), 
             size = rel(3.2)) + 
    theme(plot.margin = unit(c(-25, 0,0,-10), units = "pt"))
  
  cold_pool_panels <- ggplot() +
    geom_sf(data = sebs_layers$akland, fill = "black", 
            color = NA) +
    geom_sf(data = sebs_layers$survey.area, fill = "grey75") +
    geom_tile(data = bt_year_df,
              aes(x = x, 
                  y = y,
                  fill = temp_disc),
              color = NA) +
    geom_sf(data = sebs_layers$survey.area, 
            fill = NA, 
            color = "black") +
    geom_sf(data = sebs_layers$bathymetry) +
    geom_label(data = label_2020,
               aes(x = x,
                   y = y,
                   label = label),
               label.size = NA) +
    coord_sf(xlim = panel_extent$x, 
             ylim = panel_extent$y,
             expand = c(0,0)) +
    scale_x_continuous(name = "Longitude", 
                       breaks = c(-180, -170, -160)) + 
    scale_y_continuous(name = "Latitude", 
                       breaks = c(54, 58, 62)) +
    scale_fill_manual(name = expression("T"~(degree*C)),
                      values = rev(cpa_palette), 
                      drop = FALSE,
                      na.value = NA) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          panel.background = element_rect(color = "black", fill = NA),
          strip.text = element_text(size = 9,
                                    color = "white",
                                    face = "bold",
                                    margin = margin(0.5, 0, 0.5, 0, "mm")),
          strip.background = element_rect(fill = "#0055a4",
                                          color = NA),
          legend.position = "none") +
    facet_wrap(~year, ncol = 4)
  
  cold_pool_grid_1 <- cowplot::plot_grid(
    cold_pool_panels,
    cowplot::plot_grid(NA, cold_pool_cbar, NA,
                       nrow = 3),
    rel_widths = c(0.9,0.2)
  )
  
  # SEBS: tm_coldpool_grid_2.png ----
  
  for(i in (mid_year+1):end_year) {
    sel_layer_df <- data.frame(x = coords[,1],
                               y = coords[,2],
                               temperature = coldpool:::ebs_bottom_temperature@data@values[,i])
    sel_layer_df <- sel_layer_df[!is.na(sel_layer_df$temperature),]
    sel_layer_df <- sel_layer_df %>%
      dplyr::filter(temperature <= 2)
    sel_layer_df$year <- year_vec[i]
    
    if(i == (mid_year+1)) {
      bt_year_df <- sel_layer_df
    } else{
      bt_year_df <- dplyr::bind_rows(bt_year_df, sel_layer_df)
    }
  }
  
  bt_year_df$temp_disc <- cut(bt_year_df$temperature, 
                              breaks = c(-2, -1, 0, 1, 2))
  
  cold_pool_panels <- ggplot() +
    geom_sf(data = sebs_layers$akland, fill = "black", 
            color = NA) +
    geom_sf(data = sebs_layers$survey.area, fill = "grey75") +
    geom_tile(data = bt_year_df,
              aes(x = x, 
                  y = y,
                  fill = temp_disc),
              color = NA) +
    geom_sf(data = sebs_layers$survey.area, 
            fill = NA, 
            color = "black") +
    geom_sf(data = sebs_layers$bathymetry) +
    geom_polygon(data = data.frame(x = panel_extent$x[c(1,2,2,1,1)],
                                   y = panel_extent$y[c(1,1,2,2,1)],
                                   year = 2020),
                 aes(x = x,
                     y = y),
                 fill = "white") +
    geom_label(data = label_2020,
               aes(x = x,
                   y = y,
                   label = label),
               label.size = NA) +
    coord_sf(xlim = panel_extent$x, 
             ylim = panel_extent$y,
             expand = c(0,0)) +
    scale_x_continuous(name = "Longitude", 
                       breaks = c(-180, -170, -160)) + 
    scale_y_continuous(name = "Latitude", 
                       breaks = c(54, 58, 62)) +
    scale_fill_manual(name = expression("T"~(degree*C)),
                      values = rev(cpa_palette), 
                      drop = FALSE,
                      na.value = NA) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          panel.background = element_rect(color = "black", fill = NA),
          strip.text = element_text(size = 9,
                                    color = "white",
                                    face = "bold",
                                    margin = margin(0.5, 0, 0.5, 0, "mm")),
          strip.background = element_rect(fill = "#0055a4",
                                          color = NA),
          legend.position = "none") +
    facet_wrap(~year, ncol = 4)
  
  cold_pool_grid_2 <- cowplot::plot_grid(
    cold_pool_panels,
    cowplot::plot_grid(NA, cold_pool_cbar, NA,
                       nrow = 3),
    rel_widths = c(0.9,0.2)
  )
  
  # Setup EBS+NBS layers ----
  
  
  nbs_ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs",
                                              set.crs = coldpool:::ebs_proj_crs)
  
  year_vec <- as.numeric(gsub("[^0-9.-]", "", names(coldpool:::nbs_ebs_bottom_temperature)))
  end_year <- which(year_vec == 2021)
  
  coords <- raster::coordinates(coldpool:::nbs_ebs_bottom_temperature)
  
  for(i in 1:end_year) {
    sel_layer_df <- data.frame(x = coords[,1],
                               y = coords[,2],
                               temperature = coldpool:::nbs_ebs_bottom_temperature@data@values[,i])
    sel_layer_df <- sel_layer_df[!is.na(sel_layer_df$temperature),]
    sel_layer_df$year <- year_vec[i]
    
    if(i == 1) {
      bt_year_df <- sel_layer_df
    } else{
      bt_year_df <- dplyr::bind_rows(bt_year_df, sel_layer_df)
    }
  }
  
  # Union to combine strata 31, 32 into 30, etc.
  nbs_ebs_agg_strata <- nbs_ebs_layers$survey.strata %>%
    dplyr::mutate(agg_stratum = Stratum) %>%
    dplyr::mutate(agg_stratum = replace(agg_stratum, agg_stratum %in% c(31,32), 30),
                  agg_stratum = replace(agg_stratum, agg_stratum %in% c(41,42,43), 40),
                  agg_stratum = replace(agg_stratum, agg_stratum %in% c(61,62), 60)) %>% 
    dplyr::group_by(agg_stratum) %>% 
    dplyr::summarise()
  
  panel_extent <- data.frame(y = c(53, 67),
                             x = c(-174, -156)) %>%
    akgfmaps::transform_data_frame_crs(out.crs = coldpool:::ebs_proj_crs)
  
  nbs_ebs_temp_breaks <- c(-Inf, seq(-1,8,1), Inf)
  nbs_ebs_viridis_option <- "H" # viridis turbo palette
  n_temp_breaks <- length(nbs_ebs_temp_breaks)-1
  
  # tm_nbs_ebs_temperature_map.png ----
  
  ebs_nbs_temperature_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = nbs_ebs_layers$akland, 
                     fill = "grey70", 
                     color = "black") +
    ggplot2::geom_sf(data = nbs_ebs_layers$survey.area, fill = "grey65") +
    ggplot2::geom_tile(data = bt_year_df %>%
                         dplyr::filter(year == 2021),
                       aes(x = x, 
                           y = y,
                           fill = cut(temperature, 
                                      breaks = nbs_ebs_temp_breaks))) +
    ggplot2::geom_sf(data = nbs_ebs_agg_strata, 
                     fill = NA,
                     color = "black") +
    ggplot2::geom_text(data = data.frame(x = -158.5, 
                                         y = 62.4, 
                                         lab = "Alaska") %>%
                         akgfmaps::transform_data_frame_crs(out.crs = coldpool:::ebs_proj_crs),
                       mapping = aes(x = x,
                                     y = y,
                                     label = lab),
                       size = rel(8)) +
    ggplot2::coord_sf(xlim = panel_extent$x, 
                      ylim = panel_extent$y) +
    ggplot2::scale_x_continuous(name = "Longitude", 
                                breaks = nbs_ebs_layers$lon.breaks) + 
    ggplot2::scale_y_continuous(name = "Latitude", 
                                breaks = nbs_ebs_layers$lat.breaks) +
    ggplot2::scale_fill_manual(values = viridis_pal(option = nbs_ebs_viridis_option)(n_temp_breaks)) +
    theme_bw() +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_text(color = "black"),
                   axis.ticks = element_line(color = "black"),
                   panel.border = element_rect(color = "black", fill = NA),
                   panel.background = element_rect(color = "black", fill = NA),
                   legend.key.width = unit(12, "mm"),
                   legend.position = "none",
                   legend.direction = "horizontal", 
                   plot.margin = unit(c(5.5, 5.5,-25,5.5), units = "pt"))
  
  # tm_nbs_ebs_temperature_contour_map.png ----
  
  ebs_nbs_temperature_contour_map <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = nbs_ebs_layers$akland, 
                     fill = "grey70", 
                     color = "black") +
    ggplot2::geom_sf(data = nbs_ebs_layers$survey.area, fill = "grey65") +
    ggplot2::geom_tile(data = bt_year_df %>%
                         dplyr::filter(year == 2021),
                       aes(x = x, 
                           y = y,
                           fill = cut(temperature, breaks = nbs_ebs_temp_breaks))) +
    ggplot2::geom_contour(data = bt_year_df %>%
                            dplyr::filter(year == 2021),
                          aes(x = x, 
                              y = y,
                              z = temperature),
                          breaks = 2,
                          color = "white",
                          size = rel(1.1)) +
    ggplot2::geom_sf(data = nbs_ebs_agg_strata, 
                     fill = NA,
                     color = "black") +
    ggplot2::geom_text(data = data.frame(x = -158.5, 
                                         y = 62.4, 
                                         lab = "Alaska") %>%
                         akgfmaps::transform_data_frame_crs(out.crs = coldpool:::ebs_proj_crs),
                       mapping = aes(x = x,
                                     y = y,
                                     label = lab),
                       size = rel(8)) +
    ggplot2::coord_sf(xlim = panel_extent$x, 
                      ylim = panel_extent$y) +
    ggplot2::scale_x_continuous(name = "Longitude", 
                                breaks = nbs_ebs_layers$lon.breaks) + 
    ggplot2::scale_y_continuous(name = "Latitude", 
                                breaks = nbs_ebs_layers$lat.breaks) +
    ggplot2::scale_fill_manual(values = viridis_pal(option = nbs_ebs_viridis_option)(n_temp_breaks)) +
    theme_bw() +
    ggplot2::theme(axis.title = element_blank(),
                   axis.text = element_text(color = "black"),
                   axis.ticks = element_line(color = "black"),
                   panel.border = element_rect(color = "black", fill = NA),
                   panel.background = element_rect(color = "black", fill = NA),
                   legend.key.width = unit(12, "mm"),
                   legend.position = "none",
                   legend.direction = "horizontal", 
                   plot.margin = unit(c(5.5, 5.5,-25,5.5), units = "pt"))
  
  temp_map_cbar <- coldpool::legend_discrete_cbar(breaks = nbs_ebs_temp_breaks,
                                                  colors = viridis::viridis_pal(option = "H")(n_temp_breaks),
                                                  legend_direction = "horizontal",
                                                  font_size = 3,
                                                  width = 0.1,
                                                  expand_size.x = 0.3,
                                                  expand_size.y = 0.3,
                                                  expand.x = 0.3,
                                                  expand.y = 0.9,
                                                  spacing_scaling = 1,
                                                  text.hjust = 0.5,
                                                  font.family = "sans",
                                                  neat.labels = FALSE) + 
    annotate("text", 
             x = 1.15, 
             y = 3.5, 
             label =  expression(bold("Bottom Temperature"~(degree*C))), 
             size = rel(3.2)) + 
    theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))
  
  ebs_nbs_map <- cowplot::plot_grid(ebs_nbs_temperature_map,
                                    temp_map_cbar,
                                    nrow = 2,
                                    rel_heights = c(0.8,0.2))
  
  ebs_nbs_contour_map <- cowplot::plot_grid(ebs_nbs_temperature_contour_map,
                                            temp_map_cbar,
                                            nrow = 2,
                                            rel_heights = c(0.8,0.2))
  
  # tm_average_temperature.png ----
  
  sebs_temperatures <- coldpool:::cold_pool_index %>%
    dplyr::select(YEAR, MEAN_GEAR_TEMPERATURE, MEAN_SURFACE_TEMPERATURE) %>%
    dplyr::rename(Bottom = MEAN_GEAR_TEMPERATURE, 
                  Surface = MEAN_SURFACE_TEMPERATURE) %>%
    dplyr::mutate(group = YEAR < 2020,
                  region = "Eastern Bering Sea (summer BT survey)") %>%
    reshape2::melt(id.vars = c("YEAR", "group", "region"))
  
  nbs_temperatures <- coldpool:::nbs_mean_temperature %>%
    dplyr::select(YEAR, MEAN_GEAR_TEMPERATURE, MEAN_SURFACE_TEMPERATURE) %>%
    dplyr::rename(Bottom = MEAN_GEAR_TEMPERATURE, 
                  Surface = MEAN_SURFACE_TEMPERATURE) %>%
    dplyr::mutate(group = YEAR,
                  region = "Northern Bering Sea (summer BT survey)") %>%
    reshape2::melt(id.vars = c("YEAR", "group", "region"))
  
  all_temperatures <- dplyr::bind_rows(sebs_temperatures,
                                       nbs_temperatures)
  
  sebs_sst_mean <- mean(sebs_temperatures$value[sebs_temperatures$variable == "Surface"])
  sebs_bt_mean <- mean(sebs_temperatures$value[sebs_temperatures$variable == "Bottom"])
  nbs_sst_mean <- mean(nbs_temperatures$value[nbs_temperatures$variable == "Surface"])
  nbs_bt_mean <- mean(nbs_temperatures$value[nbs_temperatures$variable == "Bottom"])
  
  color_sst <- "darkgreen"
  color_bt <- "darkblue"
  
  ebs_mean_temp_df <- data.frame(region = c(rep("Eastern Bering Sea (summer BT survey)", 2),
                                            rep("Northern Bering Sea (summer BT survey)", 2)),
                                 variable = rep(c("Bottom", "Surface"), 2),
                                 value = c(sebs_bt_mean,
                                           sebs_sst_mean,
                                           nbs_bt_mean,
                                           nbs_sst_mean))
  
  # tm_plot_sebs_average_temperature.png ----
  
  plot_average_temperature <- ggplot(data = all_temperatures,
                                     mapping = aes(x = YEAR,
                                                   y = value,
                                                   color = variable,
                                                   shape = variable, 
                                                   group = paste0(group, variable))) +
    geom_point(size = rel(2)) +
    geom_line() +
    geom_hline(data = ebs_mean_temp_df,
               aes(yintercept = value,
                   color = variable),
               linetype = 2) +
    scale_color_manual(values = c(color_bt, color_sst)) +
    scale_y_continuous(name = expression(bold("Average Temperature"~(degree*C))), 
                       limits = c(0,11.2),
                       breaks = seq(0,12,2),
                       expand = c(0,0)) +
    scale_x_continuous(name = "Year",
                       breaks = seq(1980,2020,10)) +
    facet_wrap(~region, scales = "free") +
    theme_bw() +
    theme(axis.title = element_text(color = "black", face = "bold"),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          panel.background = element_rect(color = "black", fill = NA),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(size = 9,
                                    color = "white",
                                    face = "bold",
                                    margin = margin(0.5, 0, 0.5, 0, "mm")),
          strip.background = element_rect(fill = "#0055a4",
                                          color = NA))
  
  plot_sebs_average_temperature <- ggplot(data = all_temperatures %>%
                                            dplyr::filter(region == "Eastern Bering Sea (summer BT survey)"),
                                          mapping = aes(x = YEAR,
                                                        y = value,
                                                        color = variable,
                                                        shape = variable, 
                                                        group = paste0(group, variable))) +
    geom_point(size = rel(2)) +
    geom_line() +
    geom_hline(data = ebs_mean_temp_df %>%
                 dplyr::filter(region == "Eastern Bering Sea (summer BT survey)"),
               aes(yintercept = value,
                   color = variable),
               linetype = 2) +
    scale_color_manual(values = c(color_bt, color_sst)) +
    scale_y_continuous(name = expression(bold("Average Temperature"~(degree*C))), 
                       limits = c(0,11.2),
                       breaks = seq(0,12,2),
                       expand = c(0,0)) +
    scale_x_continuous(name = "Year",
                       breaks = seq(1980,2020,10)) +
    facet_wrap(~region, scales = "free") +
    theme_bw() +
    theme(axis.title = element_text(color = "black", face = "bold"),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          panel.border = element_rect(color = "black", fill = NA),
          panel.background = element_rect(color = "black", fill = NA),
          panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(size = 9,
                                    color = "white",
                                    face = "bold",
                                    margin = margin(0.5, 0, 0.5, 0, "mm")),
          strip.background = element_rect(fill = "#0055a4",
                                          color = NA))
  
  # Map of MEAN_BT_LT100M strata
  lt100_strata <- ebs_layers$survey.strata %>%
    dplyr::filter(Stratum %in% c(10, 20, 31, 32, 41, 42, 43)) %>%
    dplyr::group_by(SURVEY) %>%
    dplyr::summarise() %>%
    dplyr::mutate(Stratum = "MEAN_BT_LT100M Strata")
  
  panel_extent <- data.frame(y = c(53, 64),
                             x = c(-174, -156)) %>%
    akgfmaps::transform_data_frame_crs(out.crs = coldpool:::ebs_proj_crs)
  
  png(file = here::here("plots", "MEAN_BT_LT100M_strata.png"), width = 6, height = 6, units = "in", res = 120)
  print(
    ggplot() +
      ggplot2::geom_sf(data = ebs_layers$akland, 
                       fill = "grey70", 
                       color = "black") +
      geom_sf(data = lt100_strata, aes(fill = Stratum)) +
      geom_sf(data = ebs_layers$survey.strata, fill = NA) +
      geom_sf_text(data = sf::st_centroid(ebs_layers$survey.strata),
                   aes(label = Stratum)) +
      ggplot2::coord_sf(xlim = panel_extent$x, 
                        ylim = panel_extent$y) +
      ggplot2::scale_x_continuous(name = "Longitude", 
                                  breaks = ebs_layers$lon.breaks) + 
      ggplot2::scale_y_continuous(name = "Latitude", 
                                  breaks = ebs_layers$lat.breaks) +
      theme_bw() +
      ggplot2::theme(axis.title = element_blank(),
                     axis.text = element_text(color = "black"),
                     axis.ticks = element_line(color = "black"),
                     panel.border = element_rect(color = "black", fill = NA),
                     panel.background = element_rect(color = "black", fill = NA),
                     legend.key.width = unit(12, "mm"),
                     legend.position = "bottom")
  )
  dev.off()
  
  
  
  # Write plots to files ----
  png(file = here::here("plots", "tm_stacked_temperature.png"), height = 4, width = 6, units = "in", res = 600)
  print(stacked_area_plot)
  dev.off()
  
  png(file = here::here("plots", "tm_coldpool_grid_1.png"), height = 6, width = 6, units = "in", res = fig_res)
  print(cold_pool_grid_1)
  graphics.off()
  
  png(file = here::here("plots", "tm_coldpool_grid_2.png"), height = 6, width = 6, units = "in", res = fig_res)
  print(cold_pool_grid_2)
  graphics.off()
  
  png(filename = here::here("plots", "tm_nbs_ebs_temperature_map.png"), width = 6, height = 6, units = "in", res = fig_res)
  print(ebs_nbs_map)
  dev.off()
  
  png(filename = here::here("plots", "tm_nbs_ebs_temperature_contour_map.png"), width = 6, height = 6, units = "in", res = fig_res)
  print(ebs_nbs_contour_map)
  dev.off()
  
  png(file = here::here("plots", "tm_average_temperature.png"), width = 6, height = 3, units = "in", res = fig_res)
  print(plot_average_temperature)
  dev.off()
  
  png(file = here::here("plots", "tm_plot_sebs_average_temperature.png"), width = 6, height = 3, units = "in", res = fig_res)
  print(plot_sebs_average_temperature)
  dev.off()
}