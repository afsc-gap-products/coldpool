# Empirical orthogonal functions on temperature grids

# Temperature EOF
library(coldpool)
library(cowplot)
library(akgfmaps)


run_eof_cellwise <- function(x) {

  n_years <- dim(x)[3]
  
  dat_wide <- t(as.matrix(x))
  
  eof_coords <- terra::crds(x, na.rm = FALSE)
  
  # Remove NA
  dat_cols <- which(!is.na(dat_wide[1,]))
  dat_wide <- dat_wide[,dat_cols]
  eof_coords <- eof_coords[dat_cols, 1:2]
  
  # Spatial cell-wise trend
  spat_mean <- apply(dat_wide, 2, mean, na.rm = TRUE) # Spatial mean for each pixel
  Zspat_detrend <- dat_wide - outer(rep(1, n_years), spat_mean)
  Zt <- 1/sqrt(n_years-1)*Zspat_detrend

  
  # Singular value decomposition
  E <- svd(Zt)
  
  # V matrix: EOFs in space-wide format
  V <- E$v
  
  colnames(E$v) <- paste0("EOF", 1:nrow(dat_wide))
  
  # Normalized EOFs over space
  EOFs <- cbind(eof_coords, E$v) 
  
  # U matrix: Principal componen_yearss time series, where each column corresponds to a time series associated with an EOF
  TS <- data.frame(E$u) %>%
    dplyr::mutate(t = 1:nrow(E$u)) %>%
    tidyr::gather(EOF, PC, -t)
  
  # Normalized time series
  TS$nPC <- TS$PC * sqrt(n_years-1)
  
  # d matrix: diagonal element_years of D from the SVD are proportional to standard deviations from PCA
  # diag(E$d)
  
  # Plot of variance explained by EOF
  print(plot(E$d^2/sum(E$d^2), xlim = c(0, 15), type = "b", pch = 16, xlab = "EOF", 
             ylab = "Variance explained"))
  
  return(list(time_series = TS, EOFs = EOFs, EOF_var_expl = E$d^2/sum(E$d^2), svd = E, dat = dat_wide))
  
}



#' Function to plot empirical orthogonal function maps and time series -----------------------------
plot_eof <- function(eof_obj, which_eof = 1, normalize_eof = TRUE, region, years) {
  
  akgfmaps_list <- akgfmaps::get_base_layers(select.region = region, set.crs = "EPSG:3338")
  
  timeseries_df <- eof_obj$time_series
  
  eof_mat <- eof_obj$dat
  
  # Normalize EOF to correlation scale?
  if(normalize_eof) {
    eof_normalized <- vector(length = ncol(eof_mat))
    
    for(i in 1:length(eof_normalized)) {
      eof_normalized[i] <- cor(timeseries_df$PC[timeseries_df$EOF == paste0("X", which_eof)], eof_mat[,i])
    }
    sel_spatial <- data.frame(x = eof_obj$EOFs[, "x"], y = eof_obj$EOFs[, "y"], value = eof_normalized)
  } else {
    sel_spatial <- melt(eof_obj$EOFs, id.vars = c("x", "y")) %>% filter(variable %in% c(paste0("EOF", which_eof)))
  }
  
  eof_map <- 
    ggplot() + 
    geom_tile(data = sel_spatial, aes(x = x, y = y, fill = cut(value, seq(-1,1,0.2)))) +
    geom_sf(data = akgfmaps_list$akland, fill = "grey50", color = NA) +
    geom_sf(data = st_graticule(lat = seq(50,64,2), lon = seq(-180,-140, 5), margin = 1e-5),  color = alpha("grey70", 0.3), size = rel(0.3)) +
    scale_fill_manual(values = ggthemes::tableau_div_gradient_pal()(seq(0,1,length = 10)), drop = FALSE) +
    scale_x_continuous(limits = akgfmaps_list$plot.boundary$x, breaks = akgfmaps_list$lon.breaks) +
    scale_y_continuous(limits = akgfmaps_list$plot.boundary$y, breaks = akgfmaps_list$lat.breaks) +    
    theme(panel.border = element_rect(color = "black", fill = NA),
          panel.background = element_rect(fill = NA, color = "black"),
          legend.key = element_rect(fill = NA, color = "NA", size = 0.5),
          legend.position = "right",
          legend.key.width = unit(2, units = "mm"),
          legend.key.height = unit(1, units = "mm"),
          axis.title = element_blank(),
          axis.text = element_text(size = 7),
          legend.text = element_text(size = 8),
          legend.title = element_blank(),
          plot.background = element_rect(fill = NA, color = NA))
  
  time_series <- subset(timeseries_df, EOF %in% c(paste0("X", which_eof)))
  
  time_series$year <- years
  
  eof_timeseries <- ggplot(data = time_series, 
                           aes(x = year, y = nPC)) + 
    geom_line() + 
    geom_point() + 
    geom_hline(yintercept = 0, linetype = 2) +
    scale_x_continuous(name = "Year", breaks = seq(min(time_series$year), max(time_series$year), 4)) +
    scale_y_continuous(name = paste0("PC", which_eof, " (", format(100*round(eof_obj$EOF_var_expl[which_eof], 3), nsmall = 1), "%)"),
                       labels = scales::number_format(accuracy = 0.1)) +
    theme(panel.border = element_rect(color = "black", fill = NA),
          panel.background = element_rect(fill = NA, color = "black"),
          legend.key = element_rect(fill = NA, color = "grey70"),
          legend.position = "none",
          axis.title = element_text(size = 8),
          axis.text = element_text(size = 8),
          plot.background = element_rect(fill = NA, color = NA))
  
  print(cowplot::plot_grid(eof_map, eof_timeseries, nrow = 1, rel_widths = c(6,4)))
  
  # Make continuous bar legend
  
  cbar.legend <- 
    coldpool::legend_discrete_cbar(
      breaks = seq(-1,1,0.2),
      colors = ggthemes::tableau_div_gradient_pal()(seq(0,1,length = 10)),
      legend_direction = "vertical",
      font_size = 2,
      width = 1,
      expand_size.x = 0.3,
      expand_size.y = 0.1,
      expand.x = 0.1,
      expand.y = 0.8,
      spacing_scaling = 1.5,
      text.hjust = 0,
      font.family = "sans",
      neat.labels = TRUE
    )
  
  cbar.legend.horizontal <- 
    coldpool::legend_discrete_cbar(
      breaks = seq(-1,1,0.2),
      colors = ggthemes::tableau_div_gradient_pal()(seq(0,1,length = 10)),
      legend_direction = "horizontal",
      font_size = 2,
      width = 0.5,
      expand_size.x = 0.3,
      expand_size.y = 0.2,
      expand.x = 0.1,
      expand.y = 1,
      spacing_scaling = 2,
      text.hjust = 0.5,
      font.family = "sans",
      neat.labels = TRUE
    )
  
  return(list(eof_map = eof_map, eof_timeseries = eof_timeseries, cbar.legend = cbar.legend, cbar.legend.horizontal = cbar.legend.horizontal))
  
}


goa_bt <- terra::unwrap(readRDS(file = here::here("output", "GOA_bt.rds")))
goa_bt <- goa_bt[[!(names(goa_bt) == "2001")]]

goa_bt_eof <- run_eof_cellwise(x = goa_bt)

goa_bt_eof1_plot <- 
  plot_eof(
    eof_obj = goa_bt_eof,  
    region = "goa",
    which_eof = 1,
    years = as.numeric(names(goa_bt)),
    normalize_eof = TRUE
  )

goa_bt_eof1_plot$eof_timeseries
goa_bt_eof1_plot$eof_map

goa_bt_eof2_plot <- 
  plot_eof(
    eof_obj = goa_bt_eof,  
    region = "goa",
    which_eof = 2,
    years = as.numeric(names(goa_bt)),
    normalize_eof = TRUE
  )

goa_bt_eof2_plot$eof_timeseries
goa_bt_eof2_plot$eof_map


# p_goa_bt_eof <-
#   cowplot::plot_grid(
#   cowplot::plot_grid(
#     goa_bt_eof1_plot$eof_map + theme(legend.position = "none"),
#     goa_bt_eof1_plot$eof_timeseries,
#     goa_bt_eof2_plot$eof_map + theme(legend.position = "none"),
#     goa_bt_eof2_plot$eof_timeseries,
#     nrow = 2,
#     align = "hv",
#     rel_widths = c(2.2,2)
#   ),
#   cowplot::plot_grid(NULL , goa_bt_eof1_plot$cbar.legend.horizontal, NULL, ncol = 3, rel_widths = c(0.5, 3, 2)),
#   nrow = 2,
#   rel_heights = c(9,1)
# )

p_goa_bt_eof <- 
      cowplot::plot_grid(
        cowplot::plot_grid(
          goa_bt_eof1_plot$eof_map + theme(legend.position = "none"),
          goa_bt_eof1_plot$eof_timeseries,
          ncol = 2
        ),
        cowplot::plot_grid(
          goa_bt_eof2_plot$eof_map + theme(legend.position = "none"),
          goa_bt_eof2_plot$eof_timeseries,
          ncol = 2
        ),
        cowplot::plot_grid(goa_bt_eof1_plot$cbar.legend.horizontal, NULL, ncol = 2, rel_widths = c(2.2, 2)),
        nrow = 3,
        rel_heights = c(5,5,2),
        align = "h"
      )

png(here::here("plots", "GOA", "EOF_BT_GOA.png"), width = 169, height = 169/2.39, res = 300, units = "mm")
print(p_goa_bt_eof)
dev.off()

ai_bt <- terra::unwrap(readRDS(file = here::here("output", "AI_bt.rds")))

ai_bt_eof <- run_eof_cellwise(x = ai_bt)

ai_bt_eof1_plot <- 
  plot_eof(
    eof_obj = ai_bt_eof,  
    region = "ai",
    which_eof = 1,
    years = as.numeric(names(ai_bt)),
    normalize_eof = TRUE
  )

ai_bt_eof1_plot$eof_timeseries
ai_bt_eof1_plot$eof_map

ai_bt_eof2_plot <- 
  plot_eof(
    eof_obj = ai_bt_eof,  
    region = "ai",
    which_eof = 2,
    years = as.numeric(names(ai_bt)),
    normalize_eof = TRUE
  )

ai_bt_eof2_plot$eof_timeseries
ai_bt_eof2_plot$eof_map


(p_ai_bt_eof <- 
  cowplot::plot_grid(
    cowplot::plot_grid(
      ai_bt_eof1_plot$eof_map + theme(legend.position = "none"),
      ai_bt_eof1_plot$eof_timeseries,
      ncol = 2
    ),
    cowplot::plot_grid(
      ai_bt_eof2_plot$eof_map + theme(legend.position = "none"),
      ai_bt_eof2_plot$eof_timeseries,
      ncol = 2
    ),
    cowplot::plot_grid(ai_bt_eof1_plot$cbar.legend.horizontal, NULL, ncol = 2, rel_widths = c(2.2, 2)),
    nrow = 3,
    rel_heights = c(5,5,2),
    align = "h"
  ))

png(here::here("plots", "AI", "EOF_BT_AI.png"), width = 169, height = 169/2.05, res = 300, units = "mm")
print(p_ai_bt_eof)
dev.off()


