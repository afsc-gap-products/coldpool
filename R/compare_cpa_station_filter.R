#' Effect of interpolating with and without RKC resample 
#'
#' @param sel_year Year for plot
#' @param sel_old_raster File path to an old raster from a year with RKC resampling
#' @param sel_idw4_geotiff File path to a geotiff IDW raster with nmax = 4.
#' @param set_crs Coordinate reference system
#' @param temp_data_path Filed path to the temperature data .csv

compare_cpa_station_filter <- function(sel_year = 2000,
                                       sel_old_raster = "./data/idw_files/bt00_idw/",
                                       sel_idw4_geotiff = "./output/raster/idw_nmax4_2000_gear_temperature.tif",
                                       set_crs = "EPSG:3338",
                                       temp_data_path = here::here("data", list.files(here::here("data"))[length(list.files(here::here("data")))])) {
  
  # Load map layers ----
  sebs_layers <- akgfmaps::get_base_layers(select.region = "sebs",
                                           set.crs = set_crs,
                                           use.survey.bathymetry = TRUE)
  
  # RKC resample locations
  stn_locs_2000 <- read.csv(file = temp_data_path,
                            stringsAsFactors = FALSE) %>%
    akgfmaps::transform_data_frame_crs(coords = c("longitude", "latitude"),
                                       out.crs = set_crs) %>%
    dplyr::filter(year == sel_year)
  
  # Load IDW with nmax = 4, generated using R ----
  idw_nmax4 <- raster::raster(sel_idw4_geotiff) %>%
    akgfmaps::rasterize_and_mask(amask = sebs_layers$survey.area)
  
  idw_nmax4_df <- as.data.frame(idw_nmax4, xy = TRUE)
  
  plot_extent <- idw_nmax4@extent
  
  # Load old IDW raster ----
  og_idw <- raster::raster(sel_old_raster)
  og_idw_df <- as.data.frame(og_idw, xy = TRUE)
  
  # Set up temperature legend ----
  temp_breaks <- c(-Inf,-1,0,1,2,4,6,8,Inf)
  
  temp_legend <- seantools::plot_discrete_cbar(breaks = temp_breaks,
                                               colors = viridis_pal(option = "B", direction = 1)(length(temp_breaks)-1),
                                               legend_direction = "vertical",
                                               font_size = 3,
                                               width = 0.2,
                                               expand_size.x = 0.3,
                                               expand_size.y = 0.3,
                                               expand.x = 0.2,
                                               expand.y = 0.9,
                                               spacing_scaling = 1,
                                               text.hjust = 0,
                                               font.family = "sans",
                                               neat.labels = TRUE)
  
  # Bob's IDW interpolation with resample stations versus IDW (nmax = 4) without resample ----
  ifelse(!dir.exists(file.path(here::here("plots"))), dir.create(file.path(here::here("plots"))), FALSE)
  tiff(file = here::here("plots", paste0("resample_effect", sel_year, ".tiff")), 
       width = 169, 
       height = 81, 
       units = "mm",
       res = fig_res)
  print(
    cowplot::plot_grid(
      ggplot() +
        geom_tile(data = og_idw_df %>%
                    dplyr::filter(!is.na(bt00_idw)),
                  aes(x = x,
                      y = y,
                      fill = cut(bt00_idw, temp_breaks))) +
        scale_fill_viridis_d(name = expression("T"[bottom]~(degree*C)), 
                             na.value = NA,
                             drop = F,
                             option = "B") +
        geom_sf(data = sebs_layers$akland, 
                fill = "grey70") +
        geom_sf(data = sebs_layers$bathymetry, 
                size = rel(0.3)) +
        geom_point(data = stn_locs_2000 %>% 
                     dplyr::filter(haul_type == 17), 
                   aes(x = longitude, 
                       y = latitude),
                   shape = 3) +
        coord_sf(xlim = c(-600000, -200000),
                 ylim = c(5e5, 1e6)) + 
        scale_x_continuous(name = expression("Longitude"~(degree*W)),
                           expand = c(0,0),
                           breaks = c(-162, -160, -158)) +
        scale_y_continuous(name = expression("Latitude"~(degree*N)),
                           expand = c(0,0),
                           breaks = c(55, 56, 57, 58)) +
        ggtitle(label = paste0("With RKC resample (", sel_year, ")")) +
        theme_multi_map(),
      ggplot() +
        geom_tile(data = idw_nmax4_df %>%
                    filter(!is.na(idw_nmax4_2000_gear_temperature)),
                  aes(x = x, 
                      y = y, 
                      fill = cut(idw_nmax4_2000_gear_temperature, 
                                 temp_breaks))) +
        geom_sf(data = sebs_layers$akland, 
                fill = "grey70") +
        geom_sf(data = sebs_layers$bathymetry, 
                size = rel(0.3)) +
        coord_sf(xlim = c(-600000, -200000),
                 ylim = c(5e5, 1e6)) + 
        scale_x_continuous(name = expression("Longitude"~(degree*W)),
                           expand = c(0,0),
                           breaks = c(-162, -160, -158)) +
        scale_y_continuous(name = expression("Latitude"~(degree*N)),
                           expand = c(0,0),
                           breaks = c(55, 56, 57, 58)) +
        scale_fill_viridis_d(name = expression("T"[bottom]~(degree*C)), 
                             na.value = NA,
                             drop = F,
                             option = "B") +
        ggtitle(label = paste0("Without RKC resample (", sel_year, ")")) +
        theme_multi_map(),
      temp_legend +
        annotate("text", 
                 x = 1.15, 
                 y = 8.5, 
                 label =  expression("T"[bottom]~(degree*C)), 
                 size = rel(3)) +
        theme(plot.margin = unit(c(-25, 0,0,-5), units = "pt")),
      ncol = 3,
      rel_widths = c(1,1,0.3)))
  dev.off()
  
  print(paste0("Cold pool area in 2000 with RKC resample data:",
               sum(og_idw_df$bt00_idw <= 2, na.rm = TRUE)*raster::res(og_idw)[1]*raster::res(og_idw)[2]/1e6))
  
  print(paste0("Cold pool area in 2000 WITHOUT RKC resample data:",
               akgfmaps::cpa_from_raster(idw_nmax4, temperature_threshold = 2)))
  
}
