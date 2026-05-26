library(akgfmaps)
library(coldpool)
library(tidyterra)


cor.test(c(NA, runif(10)), runif(11), use = "na.or.complete")

scaled_corr <- function(x, y, use) {
  
  cor.test(scale(x), scale(y))
  cor(scale(x), scale(y), use = "na.or.complete")
}

scaled_corr_p <- function(x, y) {

  keep <- !is.na(x) & !is.na(y)
  x_clean <- x[keep]
  y_clean <- y[keep]
  
  if (length(x_clean) < 3 || var(x_clean) == 0 || var(y_clean) == 0) {
    return(c(correlation = NA_real_, p_value = NA_real_))
  }
  
  test_out <- cor.test(as.numeric(scale(x_clean)), as.numeric(scale(y_clean)))
  
  return(c(
    correlation = as.numeric(test_out$estimate), 
    p_value     = test_out$p.value
  ))
}

calc_corr <- 
  function(bt_stack, sst_stack) {
    
    corr_rast <- terra::xapp(
      bt_stack, sst_stack, fun = scaled_corr_p
    )
    
    names(corr_rast) <- c("r", "p_value")
    
    return(corr_rast)
  }


# Calculate correlations
ai_corr_stack <- 
  calc_corr(
    bt_stack = terra::unwrap(
      readRDS(
        here::here("output", "AI_bt.rds")
      )
    ),
    sst_stack = terra::unwrap(
      readRDS(
        here::here("output", "AI_sst.rds")
      )
    )
  )

goa_corr_stack <- 
  calc_corr(
    bt_stack = terra::unwrap(
      readRDS(
        here::here("output", "GOA_bt.rds")
      )
    ),
    sst_stack = terra::unwrap(
      readRDS(
        here::here("output", "GOA_sst.rds")
      )
    )
  )

# Get layers 

ai_layers <- akgfmaps::get_base_layers(select.region = "ai", set.crs = crs(ai_corr_stack))
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = crs(goa_corr_stack))

plot(goa_corr_stack)



make_signif_layer <- 
  function(x, level = 0.05) {
    
    x <- x['p_value']
    
    values(x) <- values(x) < level
    
    x <- x |> 
      terra::as.polygons() |>
      sf::st_as_sf() |>
      dplyr::filter(p_value == 1)
    
    return(x)
  }




# Depth-dependent correlation

# Temperature anomalies are more strongly correlated in shallow areas and at depth; 

p_corr_goa <- 
  ggplot() +
  geom_sf(data = goa_layers$akland, color = NA, fill = "grey70") +
  geom_spatraster(
    data = goa_corr_stack,
    mapping = aes(fill = r)
  ) +
  geom_sf(
    data = make_signif_layer(goa_corr_stack), 
    fill = NA, 
    mapping = aes(color = "True"), 
    linewidth = 0.4
  ) +
  scale_fill_distiller(name = "r", palette = "BrBG", na.value = NA, limits = c(-1,1)) +
  scale_color_manual(values = "red", name = "p<0.05") +
  scale_x_continuous(limits = goa_layers$plot.boundary$x, breaks = goa_layers$lon.breaks) +
  scale_y_continuous(limits = goa_layers$plot.boundary$y, breaks = goa_layers$lat.breaks) +
  theme_bw()

p_corr_ai <- 
  ggplot() +
  geom_sf(data = ai_layers$akland, color = NA, fill = "grey70") +
  geom_spatraster(
    data = ai_corr_stack,
    mapping = aes(fill = r)
  ) +
  geom_sf(
    data = make_signif_layer(ai_corr_stack), 
    fill = NA, 
    mapping = aes(color = "True"), 
    linewidth = 0.4
  ) +
  scale_color_manual(values = "red", name = "p<0.05") +
  scale_fill_distiller(name = "r", palette = "BrBG", na.value = NA, limits = c(-1,1)) +
  scale_x_continuous(limits = ai_layers$plot.boundary$x, breaks = ai_layers$lon.breaks) +
  scale_y_continuous(limits = ai_layers$plot.boundary$y, breaks = ai_layers$lat.breaks) +
  theme_bw()

corr_legend <- cowplot::get_legend(p_corr_ai)

p_corr_maps <- 
  cowplot::plot_grid(
    cowplot::plot_grid(
      p_corr_ai + theme(legend.position = "none"),
      p_corr_goa + theme(legend.position = "none"),
      nrow = 2,
      align = "v",
      rel_heights = c(1.1, 1)
    ),
    corr_legend,
    ncol = 2, 
    rel_widths = c(9, 1.1)
  )

png(here::here("plots", "corr_map_both_regions.png"), width = 168, height = 168/1.51, units = "mm", res = 300)
print(p_corr_maps)
dev.off()

