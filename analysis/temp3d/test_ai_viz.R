library(coldpool)

# AI temperature bar plot
ai_bt <- readRDS(here::here("analysis", "temp3d", "output", "AI_bt.rds"))

ai_esr_subareas <- 
  akgfmaps::get_esr_regions(select.region = "esr_subarea", set.crs = "EPSG:3338") |>
  dplyr::filter(grepl(pattern = "Aleutians", x = AREA_NAME))

ai_year_breaks <- seq(1994, lubridate::year(Sys.Date()), 2)

min_year <- min(as.numeric(names(ai_bt)))
max_year <- max(as.numeric(names(ai_bt)))

temp_breaks <- 
  matrix(
    c(-99, 3.5, 1,
      3.5, 4, 2,
      4, 4.5, 3,
      4.5, 5, 4,
      5, 5.5, 5,
      5.5, 6, 6,
      6, 99, 7),
    ncol = 3,
    byrow = TRUE
  )

bt_cbar_breaks <- c(-Inf, seq(3.5, 6, 0.5), Inf)

bt_labels <- c("<3.5", "3.5-4.0", "4.0-4.5", "4.5-5.0", "5.0-5.5",  "5.5-6.0", ">6.0")

ai_bt_breaks <- classify(ai_bt, temp_breaks, include.lowest = FALSE, others = NA)

bt_layers <- 
  lapply(
    ai_bt_breaks, 
    FUN = function(x) {
      sel_bt <-  x |>
        terra::as.polygons() |>
        sf::st_as_sf()
      
      sel_bt$year <- names(sel_bt)[1]
      
      names(sel_bt)[1] <- "bt_level"
      
      sel_bt
    }
  )

# Bottom temperature for the full AI ----

total_area <- 
  (do.call(what = dplyr::bind_rows, bt_layers)  |>
  dplyr::filter(year == 1994) |>
  dplyr::mutate(area_km2 = as.numeric(sf::st_area(geometry)/1e6)) |>
  sf::st_drop_geometry() |>
  dplyr::group_by(year)  |>
  dplyr::summarise(total_area_km2 = sum(area_km2)))$total_area_km2

bt_poly <- 
  do.call(what = dplyr::bind_rows, bt_layers) |>
  dplyr::mutate(
    area_km2 = as.numeric(sf::st_area(bt_poly))/1e6,
    proportion = area_km2/total_area,
    bt_label = factor(bt_level, labels = bt_labels)
  )

bt_cbar <- 
  coldpool::legend_discrete_cbar(
    breaks = bt_cbar_breaks,
    colors = viridis::viridis_pal(option = "H")(length(bt_cbar_breaks)-1),
    legend_direction = "horizontal",
    font_size = 3,
    width = 0.1,
    expand_size.x = 0.3,
    expand_size.y = 0.3,
    expand.x = 0.3,
    expand.y = 0.9,
    spacing_scaling = 1,
    text.hjust = 0.5,
    text.vjust = 0.7,
    font.family = "sans",
    neat.labels = FALSE
  ) + 
  annotate(
    "text", 
    x = 1.2, 
    y = 3.5, 
    label =  "Bottom Temperature (\u00B0C)",
    size = rel(3)
  ) + 
  theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))


p_ai_bt_bar1 <- 
  ggplot() +
  geom_bar(
    data = bt_poly,
    mapping = aes(
      x = as.numeric(year), 
      y = area_km2, 
      fill = bt_label, group = year),
    stat = "identity",
    position = "stack",
    width = 1
  ) +
  scale_fill_viridis_d(name = expression('BT ('*degree*C*')'), option = "H") +
  scale_x_continuous(name = "Year", expand = c(0,0), breaks = ai_year_breaks) +
  scale_y_continuous(name = expression('Area ('*km^2*')'), expand = c(0,0)) +
  facet_wrap(~"Aleutian Islands") +
  coldpool::theme_timeseries_blue_strip() +
  theme(legend.position = "none",
        plot.margin = unit(c(5,5,-10,5), units = "pt"))

p_ai_bt_bar2 <- 
  ggplot() +
  geom_bar(
    data = bt_poly,
    mapping = aes(
      x = as.numeric(year), 
      y = proportion, 
      fill = bt_label, group = year),
    stat = "identity",
    position = "stack",
    width = 1
  ) +
  scale_fill_viridis_d(name = expression('BT ('*degree*C*')'), option = "H") +
  scale_x_continuous(name = "Year", expand = c(0,0), breaks = ai_year_breaks) +
  scale_y_continuous(name = "Proportion of Survey Area", expand = c(0,0)) +
  facet_wrap(~"Aleutian Islands") +
  coldpool::theme_timeseries_blue_strip() +
  theme(legend.position = "none",
        plot.margin = unit(c(5,5,-10,5), units = "pt"))


png(filename = here::here("plots", "ai", paste0(max_year, "_ai_bt_area.png")), res = 300, 
    width = 6, height = 3, units = "in")
print(
  cowplot::plot_grid(
    p_ai_bt_bar1,
    bt_cbar,
    nrow = 2, 
    rel_heights = c(0.75, 0.25)
  )
)
dev.off()

png(filename = here::here("plots", "ai", paste0(max_year, "_ai_bt_proportion.png")), res = 300, 
    width = 6, height = 3, units = "in")
print(
  cowplot::plot_grid(
    p_ai_bt_bar2,
    bt_cbar,
    nrow = 2, 
    rel_heights = c(0.75, 0.25)
  )
)
dev.off()

# Bottom temperature by subarea ----

bt_subarea <- 
  lapply(
    ai_bt_breaks, 
    FUN = function(x) {
      sel_bt <-  x |>
        terra::as.polygons() |>
        sf::st_as_sf() |>
        sf::st_intersection(ai_esr_subareas)
      
      sel_bt$year <- gsub("[^0-9]", "", names(sel_bt)[1])
      
      names(sel_bt)[1] <- "bt_level"
      
      sel_bt
    }
  )

# Bottom temperature by subarea ----

ai_subarea_order <- c("Western Aleutians", "Central Aleutians", "Eastern Aleutians")

# Calculate total subarea
subarea_total_area <- 
  do.call(what = dplyr::bind_rows, bt_subarea)  |>
  dplyr::filter(year == 1994) |>
  dplyr::mutate(
    area_km2 = as.numeric(sf::st_area(geometry)/1e6),
    AREA_NAME = factor(AREA_NAME, levels = ai_subarea_order)
    ) |>
  sf::st_drop_geometry() |>
  dplyr::group_by(AREA_NAME)  |>
  dplyr::summarise(total_area_km2 = sum(area_km2))

# Calculate area for each temperature level and calculate proportions
bt_subarea_poly <-
  do.call(what = dplyr::bind_rows, bt_subarea) |>
  dplyr::mutate(
    area_km2 = as.numeric(sf::st_area(geometry)/1e6),
    bt_label = factor(bt_level, labels = bt_labels),
    AREA_NAME = factor(AREA_NAME, levels = ai_subarea_order)
  ) |>
  sf::st_drop_geometry() |>
  dplyr::group_by(year, AREA_NAME, bt_level, bt_label) |>
  dplyr::summarise(area_km2 = sum(area_km2)) |>
  dplyr::inner_join(
    subarea_total_area
  ) |>
  dplyr::mutate(
    proportion = area_km2/total_area_km2
  )

bt_subarea_cbar <- 
  coldpool::legend_discrete_cbar(
    breaks = bt_cbar_breaks,
    colors = viridis::viridis_pal(option = "H")(length(bt_cbar_breaks)-1),
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
    neat.labels = FALSE
  ) + 
  annotate(
    "text", 
    x = 1.15, 
    y = 3.5, 
    label =  "Bottom Temperature (\u00B0C)",
    size = rel(3.2)
  ) + 
  theme(plot.margin = unit(c(0,0, 0, 5), units = "mm"))

p_ai_subarea_bt_bar1 <- 
  ggplot() +
  geom_bar(
    data = bt_subarea_poly,
    mapping = aes(
      x = as.numeric(year), 
      y = area_km2, 
      fill = bt_label, group = year),
    stat = "identity",
    position = "stack",
    width = 1
  ) +
  scale_fill_viridis_d(name = expression('BT ('*degree*C*')'), option = "H") +
  scale_x_continuous(name = "Year", expand = c(0,0), breaks = ai_year_breaks) +
  scale_y_continuous(name = expression('Area ('*km^2*')'), expand = c(0,0)) +
  facet_wrap(~AREA_NAME,  scales = "free_y", nrow = 3) +
  coldpool::theme_timeseries_blue_strip() +
  coldpool::theme_timeseries_blue_strip() +
  theme(legend.position = "none",
        plot.margin = unit(c(5,5,-20,5), units = "pt"))

p_ai_subarea_bt_bar2 <- 
  ggplot() +
  geom_bar(
    data = bt_subarea_poly,
    mapping = aes(
      x = as.numeric(year), 
      y = proportion, 
      fill = bt_label, group = year),
    stat = "identity",
    position = "stack",
    width = 1
  ) +
  scale_fill_viridis_d(name = expression('BT ('*degree*C*')'), option = "H") +
  scale_x_continuous(name = "Year", expand = c(0,0), breaks = ai_year_breaks) +
  scale_y_continuous(name = "Proportion of Survey Area", expand = c(0,0)) +
  facet_wrap(~AREA_NAME, scales = "free_y", nrow = 3) +
  coldpool::theme_timeseries_blue_strip() +
  coldpool::theme_timeseries_blue_strip() +
  theme(legend.position = "none",
        plot.margin = unit(c(5,5,-20,5), units = "pt"))


png(filename = here::here("plots", "ai", paste0(max_year, "_ai_bt_area_by_subarea.png")), res = 300, 
    width = 6.5, height = 6, units = "in")
print(
  cowplot::plot_grid(
    p_ai_subarea_bt_bar1,
    bt_subarea_cbar,
    nrow = 2, rel_heights = c(0.85,0.15)
  )
)
dev.off()


png(filename = here::here("plots", "ai", paste0(max_year, "_ai_bt_proportion_by_subarea.png")), res = 300, 
    width = 6.5, height = 6, units = "in")
print(
  cowplot::plot_grid(
    p_ai_subarea_bt_bar2,
    bt_subarea_cbar,
    nrow = 2, rel_heights = c(0.85,0.15)
  )
)
dev.off()

