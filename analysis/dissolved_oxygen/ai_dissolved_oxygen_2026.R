# Dissolved Oxygen maps for September 2026 GPT

library(RNetCDF)
library(ncdf4)
library(coldpool)
library(stars)
library(akgfmaps)
library(tidyterra)

dir.create(here::here("analysis", 
                      "dissolved_oxygen",
                      "plots"),
           showWarnings = FALSE)

fig_res <- 300

ai_years <- c(2024, 2026)

do_breaks <- c(-Inf, seq(2,10,2), Inf)

map_layers <- akgfmaps::get_base_layers(
  select.region = "ai", 
  set.crs = "EPSG:3338"
)

con <- ncdf4::nc_open(here::here("analysis", "dissolved_oxygen", "data", "GAPCTD_2024_AI.nc"))

do_df <- data.frame(sea_floor_dissolved_oxygen = ncvar_get(con, "sea_floor_dissolved_oxygen"),
                    latitude = ncvar_get(con, "latitude"),
                    longitude = ncvar_get(con, "longitude"),
                    stationid = ncvar_get(con, "stationid"),
                    depth = ncvar_get(con, "haul_depth"),
                    year = 2024)

con <- ncdf4::nc_open(here::here("analysis", "dissolved_oxygen", "data", "GAPCTD_2026_AI.nc"))

do_df <- 
  dplyr::bind_rows(do_df,
                   data.frame(sea_floor_dissolved_oxygen = ncvar_get(con, "sea_floor_dissolved_oxygen"),
                              latitude = ncvar_get(con, "latitude"),
                              longitude = ncvar_get(con, "longitude"),
                              stationid = ncvar_get(con, "stationid"),
                              depth = ncvar_get(con, "haul_depth"),
                              year = 2026)
  )

do_sf <- sf::st_as_sf(do_df, coords = c("longitude", "latitude"), crs = "WGS84") |>
  sf::st_transform(crs = "EPSG:3338")

do_sf$do_mg_l <- do_sf$sea_floor_dissolved_oxygen * 1.428

p_ai_map <- 
  ggplot() +
  geom_sf(data = map_layers$akland) +
  geom_sf(data = map_layers$bathymetry, color = "grey70", linewidth = 0.15) +
  geom_sf(data = do_sf |> dplyr::arrange(do_mg_l),
          mapping = aes(color = cut(do_mg_l, breaks = do_breaks, right = TRUE)),
          alpha = 0.85,
          size = rel(0.8)
  ) +
  geom_sf(data = dplyr::filter(do_sf, do_mg_l < 2),
          color = "red",
          shape = 21, fill = NA, size = rel(4)) +
  geom_sf(data = map_layers$graticule, linewidth = 0.3, alpha = 0.2) +
  scale_color_brewer(drop = FALSE, na.value = NA) +
  facet_wrap(~year, nrow = 2) +
  coord_sf(xlim = map_layers$plot.boundary$x,
           ylim = map_layers$plot.boundary$y) +
  scale_x_continuous(breaks = map_layers$lon.breaks) +
  scale_y_continuous(breaks = map_layers$lat.breaks) +
  coldpool::theme_multi_map_blue_strip() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(5,0,5,5), units = "mm"),
        axis.title = element_blank(),
        axis.text = element_text(size = 9))

ggplot() +
  geom_point(
    data = dplyr::filter(do_sf, depth >0),
    mapping = aes(x = depth, y = do_mg_l, color = factor(year))
  ) +
  geom_hline(yintercept = 2, linetype = 2) +
  scale_x_continuous(name = "Depth (m)") +
  scale_y_continuous(name = "Dissolved oxygen (mg/l)")

ggplot() +
  geom_boxplot(
    data = dplyr::filter(do_sf, depth >0),
    mapping = aes(x = cut(depth, seq(0,500,50)), y = do_mg_l, color = factor(year))
  ) +
  # geom_hline(yintercept = 2, linetype = 2) +
  scale_x_discrete(name = "Depth (m)") +
  scale_y_continuous(name = "Dissolved oxygen (mg/l)")

ggplot() +
  geom_sf(data = map_layers$akland) +
  geom_sf(data = map_layers$bathymetry, color = "grey70", linewidth = 0.15) +
  geom_sf(data = do_sf#,
          # size = rel(0.5)
  ) +
  geom_sf(data = dplyr::filter(do_sf, do_mg_l <= 2),
          mapping = aes(color = "Hypoxia (<2 mg/l)")#,
          # size = rel(0.8)
  ) +
  geom_sf(data = map_layers$graticule, linewidth = 0.3, alpha = 0.2) +
  scale_color_manual(values = "red") +
  facet_wrap(~year, nrow = 2) +
  coord_sf(xlim = map_layers$plot.boundary$x,
           ylim = map_layers$plot.boundary$y) +
  scale_x_continuous(breaks = map_layers$lon.breaks) +
  scale_y_continuous(breaks = map_layers$lat.breaks) +
  coldpool::theme_multi_map_blue_strip() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(5,5,5,5), units = "mm"),
        axis.title = element_blank(),
        axis.text = element_text(size = 9))


cbar_legend <- 
  coldpool::legend_discrete_cbar(
    breaks = do_breaks,
    colors = scales::brewer_pal()(length(do_breaks)-1),
    # colors = viridis::viridis_pal(option = "rocket",
    #                               direction = -1),
    legend_direction = "vertical",
    font_size = 4.5,
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
  annotate("text", 
           x = 1.25, 
           # y = 4, 
           y = 10.25,
           label = "Bottom DO (mg\u00B7l\u207B\u00B9)", 
           size = rel(4.2),
           fontface = "bold") + 
  theme(plot.margin = unit(c(2,0, 0, -6), units = "mm"))

do_map_grid <- 
  cowplot::plot_grid(
    p_ai_map + theme(strip.text = element_text(size = 16), axis.text = element_text(size = 13)), 
    cbar_legend,
    nrow = 1, 
    ncol = 2,
    rel_widths = c(0.8, 0.25)
  )

ragg::agg_png(filename = here::here("analysis", "dissolved_oxygen", "plots",
                                    paste0("2026_ai_dissolved_oxygen_map.png")), width = 8.5, height = 6.25, units = "in", res = fig_res)
print(do_map_grid)
dev.off()

