# Map of ESR subregions and survey areas

library(akgfmaps)
library(shadowtext)
library(ggrepel)

bts_layers <- akgfmaps::get_base_layers(select.region = c("ai", "goa"), set.crs = 3338)

# goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = 3338)

esr_ecoregions <- akgfmaps::get_esr_regions(set.crs = 3338) |>
  dplyr::inner_join(
    data.frame(
      AREA_NAME = c("Central Aleutians", "Western Aleutians", "Eastern Aleutians", "Western Gulf of Alaska", "Eastern Gulf of Alaska"),
      AREA_ABBV = c("CAI", "WAI", "EAI", "WGOA", "EGOA"),
      by = "AREA_NAME"
    )
  )

nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = 3338)

nmfs_area_labels <- 
  nmfs_areas |>
  sf::st_centroid() |>
  dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                y = sf::st_coordinates(geometry)[,2]) |>
  sf::st_drop_geometry()

esr_ecoregion_labels <- 
  esr_ecoregions |>
  sf::st_centroid() |>
  dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                y = sf::st_coordinates(geometry)[,2]) |>
  sf::st_drop_geometry()


area_map_1 <-
  ggplot() +
  geom_sf(data = esr_ecoregions,
          mapping = aes(fill = AREA_ABBV),
          alpha = 0.3,
          color = NA) +
  geom_sf(
    data = bts_layers$survey.area,
    mapping = aes(color = c("GOA BTS", "AI BTS")),
    fill = NA,
    linewidth = rel(1.02)
  ) +
  geom_sf(
    data = nmfs_areas,
    mapping = aes(linetype = "NMFS Stat. Area"),
    fill = NA
  ) +
  geom_sf(data = bts_layers$akland, color = NA, fill = "grey70") +
  geom_shadowtext(
    data = nmfs_area_labels,
    mapping = aes(x = x, y = y, label = REP_AREA),
    bg.color = "white",
    color = "black"
  ) +
  scale_x_continuous(limits = bts_layers$plot.boundary$x + c(-3e5, + 0),
                     breaks = bts_layers$lon.breaks) +
  scale_y_continuous(limits = bts_layers$plot.boundary$y + c(-3e5, + 0),
                     breaks = bts_layers$lat.breaks) +
  scale_fill_manual(
    name = "ESR Ecoregion",
    values = c(
      "WAI" = "#FDE725FF", 
      "CAI" = "#5DC863FF", 
      "EAI" = "#21908CFF",
      "WGOA" = "#3B528BFF",
      "EGOA" = "#440154FF"
    )
  ) +
  scale_color_manual(
    name = "BTS Area",
    values = c("AI BTS" = "#E69F00", 
               "GOA BTS" = "#CC79A7")
  ) +
  scale_linetype_manual(values = c("NMFS Stat. Area" = 1)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        axis.title = element_blank(),
        legend.position = "bottom")



area_map_2 <- 
  ggplot() +
  geom_sf(
    data = bts_layers$akland, color = NA, fill = "grey70"
  ) +
  geom_sf(
    data = bts_layers$survey.area,
    mapping = aes(fill = c("GOA BTS", "AI BTS")),
    color = NA,
    alpha = 0.7
  ) +
  geom_sf(
    data = esr_ecoregions,
    mapping = aes(color = AREA_ABBV),
    fill = NA,
    linewidth = 0.4
  ) +
  geom_shadowtext(
    data = esr_ecoregion_labels,
    mapping = aes(x = x, y = y, color = AREA_ABBV, label = AREA_ABBV),
    bg.color = "white",
    size = 4
  ) +
  scale_x_continuous(
    limits = bts_layers$plot.boundary$x + c(-3e5, + 0),
    breaks = bts_layers$lon.breaks
  ) +
  scale_y_continuous(
    limits = bts_layers$plot.boundary$y + c(-3e5, + 0),
    breaks = bts_layers$lat.breaks
  ) +
  scale_color_manual(
    values = c(
      "WAI" = "#8FD744FF", 
      "CAI" = "#35B779FF", 
      "EAI" = "#21908CFF",
      "WGOA" = "#31688EFF",
      "EGOA" = "#440154FF"
    ),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      "AI BTS" = "#E69F00", 
      "GOA BTS" = "#CC79A7"
    )
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.2),
    legend.text = element_text(size = 7),
    legend.key.size = unit(3.5, units = "mm"),
    axis.title = element_blank(),
    panel.grid = element_line(linewidth = 0.4)
  )


png(here::here("plots", "area_map.png"), width = 115, height = 115/2.5, units = "mm", res = 300)
print(area_map_2)
dev.off()
