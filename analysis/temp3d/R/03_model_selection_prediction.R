library(coldpool)

model_performance_table <- 
  readRDS(
    here::here("output", region, paste0(region, "_splm_interp_performance.rds"))
  )

# Model selection ----
# Compare models based on RMSPE
model_performance_table |>
  dplyr::group_by(region, layer, spcov_type, anisotropy, formula) |>
  dplyr::summarise(mean_bias = mean(bias),
                   mean_mspe = mean(mspe),
                   mean_rmspe = mean(rmspe),
                   mean_cor2 = mean(cor2),
                   n_best = sum(best)) |>
  dplyr::mutate(form_char = as.character(formula)) |>
  dplyr::filter(n_best > 0) |>
  dplyr::arrange(layer, mean_rmspe)

# Make model performance table ---------------------------------------------------------------------

model_summary <- 
  dplyr::bind_rows(
    readRDS(here::here("output", "ai", paste0("ai_splm_interp_performance.rds"))),
    readRDS(here::here("output", "goa", paste0("goa_splm_interp_performance.rds")))
  ) |>
  dplyr::mutate(form_char = as.character(formula)) |>
  dplyr::inner_join(
    data.frame(
      form_char = c(
        "GEAR_TEMPERATURE_C ~ I(log(DEPTH_M))",
        "GEAR_TEMPERATURE_C ~ DEPTH_M + I(DEPTH_M^2)",
        "GEAR_TEMPERATURE_C ~ DEPTH_M",
        "GEAR_TEMPERATURE_C ~ 1",
        "SURFACE_TEMPERATURE_C ~ I(log(DEPTH_M))",
        "SURFACE_TEMPERATURE_C ~ DEPTH_M + I(DEPTH_M^2)",
        "SURFACE_TEMPERATURE_C ~ DEPTH_M",
        "SURFACE_TEMPERATURE_C ~ 1"
      ),
      form_lab = c("log(depth)", "depth + depth^2", "depth", "none")
    )
  ) |>
  dplyr::mutate(
    form_lab = factor(form_lab, levels = c("none", "depth", "depth + depth^2", "log(depth)"))
  )


best_model_tally <- 
  model_summary |>
  dplyr::group_by(region, layer, spcov_type, anisotropy, form_lab, form_char) |>
  dplyr::summarise(mean_bias = mean(bias),
                   mean_mspe = mean(mspe),
                   mean_rmspe = mean(rmspe),
                   mean_cor2 = mean(cor2),
                   n_best = sum(best)) |>
  dplyr::filter(n_best > 0) |>
  dplyr::arrange(layer, region, mean_rmspe)

# Create best model table
best_model_tally |>
  dplyr::mutate(anisotropy = dplyr::if_else(anisotropy == TRUE, "Yes", "No"),
                layer = paste0(region, " _", layer),
                mean_rmspe = format(round(mean_rmspe, 3), nsmall = 3)) |>
  dplyr::ungroup() |>
  dplyr::select(Layer = layer, `Spatial covariance` = spcov_type, anisotropy, Years = n_best, `Depth effect` = form_lab, RMSPE = mean_rmspe, bias = mean_bias, cor2 = mean_cor2) |>
  write.csv(file = here::here("plots", "best_model_table.csv"), row.names = FALSE)

ggplot() +
  geom_boxplot(data = 
                 best_model_tally |>
                 dplyr::inner_join(model_summary),
               mapping = aes(x = paste0(spcov_type, dplyr::if_else(anisotropy, "-anis.", "")),
                             y = rmspe)) +
  scale_x_discrete(name = "Model") +
  scale_y_continuous(name = "Annual MSPE") +
  theme_bw()

p_rmspe_bt <-
  ggplot() +
  geom_point(data = 
               dplyr::filter(model_summary, layer == "bottom") |>
               dplyr::arrange(rmspe),
             mapping = aes(x = YEAR,
                           y = rmspe,
                           color = spcov_type,
                           shape = anisotropy)) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "RMSPE (bottom)") +
  scale_shape(name = "Anisotropy", solid = FALSE) +
  scale_color_colorblind(name = "Sp. cov.") +
  facet_grid(region~form_lab, scales = "free_y") +
  theme_bw()

p_rmspe_st <- 
  ggplot() +
  geom_point(data = 
               dplyr::filter(model_summary, layer == "surface") |>
               dplyr::arrange(rmspe),
             mapping = aes(x = YEAR,
                           y = rmspe,
                           color = spcov_type,
                           shape = anisotropy)) +
  scale_x_continuous(name = "Year") +
  scale_y_continuous(name = "RMSPE (surface)") +
  scale_shape(name = "Anisotropy", solid = FALSE) +
  scale_color_colorblind(name = "Sp. cov.") +
  facet_grid(region~form_lab, scales = "free_y") +
  theme_bw()

p_legend <- cowplot::get_legend(p_rmspe_bt, return_all = TRUE)

p_annual_rmspe_grid <- 
  cowplot::plot_grid(
    cowplot::plot_grid(
      p_rmspe_st + 
        theme(legend.position = "none", axis.title.x = element_blank()),
      p_rmspe_bt + theme(legend.position = "none"),
      nrow = 2,
      align = "v"
    ),
    p_legend,
    ncol = 2,
    rel_widths = c(0.85, 0.15)
  )

png(
  here::here("plots", "rmspe_annual_grid.png"), 
  width = 169, 
  height = 140, 
  units = "mm",
  res = 300)
print(p_annual_rmspe_grid)
dev.off()

# Best models based on RMSPE
# GOA bottom: Circular bottom depth model with anisotropy and log(DEPTH_M)
# GOA surface: Circular model with anisotropy and quadratic effect of depth
# AI bottom: Matern model with anisotropy and log(DEPTH_M)
# AI surface: Several models are more or less tied based on RMSPE; circular model with anisotropy and quadratic effect of depth

# Set best model
if(all(survey_definition_id == 47)) {
  bt_formula <- formula("GEAR_TEMPERATURE_C ~ I(log(DEPTH_M))")
  bt_spcov_type <- "circular"
  bt_anisotropy <- TRUE
  
  sst_formula <- formula("SURFACE_TEMPERATURE_C ~ DEPTH_M")
  sst_spcov_type <- "circular"
  sst_anisotropy <- TRUE
}

if(all(survey_definition_id == 52)) {
  bt_formula <- formula("GEAR_TEMPERATURE_C ~ I(log(DEPTH_M))")
  bt_spcov_type <- "matern"
  bt_anisotropy <- TRUE
  
  sst_formula <- formula("SURFACE_TEMPERATURE_C ~ DEPTH_M + I(DEPTH_M^2)")
  sst_spcov_type <- "circular"
  sst_anisotropy <- TRUE
}

# Load spatial data that are needed to format and predict temperature within BTS areas

map_layers <- 
  akgfmaps::get_base_layers(
    select.region = region, 
    set.crs = coldpool::ebs_proj_crs
  )

esr_subareas <-
  akgfmaps::get_esr_regions(select.region = "esr_subarea", set.crs = "EPSG:3338") |>
  dplyr::filter(AREA_NAME %in% subarea_levels)

bathy <- 
  system.file("extdata", "bathymetry.gpkg", package = "akgfmaps") |>
  terra::rast() |>
  terra::mask(map_layers$survey.area) |>
  terra::trim() 

# Create a UTM 
bathy_utm <- bathy |>
  terra::as.points(na.rm = TRUE) |>
  sf::st_as_sf() |>
  dplyr::rename(DEPTH_M = Height) |>
  sf::st_transform(crs = utmcrs)

# Predict temperatures in UTM CRS, convert back to AEA CRS -----------------------------------------
haul_data <- readRDS(here::here("data", region, paste0(region, "_akfin_haul.rds"))) |>
  dplyr::filter(GEAR_TEMPERATURE_C > min_temp)

haul_data$LATITUDE_DD_END[is.na(haul_data$LATITUDE_DD_END)] <- haul_data$LATITUDE_DD_START[is.na(haul_data$LATITUDE_DD_END)]
haul_data$LONGITUDE_DD_END[is.na(haul_data$LONGITUDE_DD_END)] <- haul_data$LONGITUDE_DD_START[is.na(haul_data$LONGITUDE_DD_END)]

towpath <- vector(mode = "list", length = length(haul_data))

for(ii in 1:nrow(haul_data)) {
  
  towpath[[ii]] <- c(
    sf::st_point(c(
      haul_data$LONGITUDE_DD_START[ii], 
      haul_data$LATITUDE_DD_START[ii])),
    sf::st_point(c(
      haul_data$LONGITUDE_DD_END[ii], 
      haul_data$LATITUDE_DD_END[ii]))
  ) |>
    sf::st_linestring()
  
}

towpath <- towpath |>
  sf::st_as_sfc(crs = "WGS84")

haul_data <- sf::st_sf(haul_data, geometry = towpath) |>
  navmaps::st_line_midpoints() |>
  sf::st_transform(crs = utmcrs)

start_time <- Sys.time()

unique_years <- sort(unique(haul_data$YEAR))

for(jj in 1:length(unique_years)) {
  
  sel_dat <- dplyr::filter(haul_data, YEAR == unique_years[jj])
  
  best_bt_model <- 
    dplyr::filter(
      model_performance_table, 
      YEAR == unique_years[jj],
      layer == "bottom",
      best == TRUE
    )
  
  bt_mod <- 
    spmodel::splm(
      formula = bt_formula, 
      data = sel_dat, 
      spcov_type = bt_spcov_type,
      anisotropy = bt_anisotropy
    )
  
  best_sst_model <- 
    dplyr::filter(
      model_performance_table, 
      YEAR == unique_years[jj],
      layer == "surface",
      best == TRUE
    )
  
  sst_mod <- 
    spmodel::splm(
      formula = sst_formula, 
      data = sel_dat, 
      spcov_type = sst_spcov_type,
      anisotropy = sst_anisotropy
    )
  
  bathy_utm[["BT"]] <- predict(bt_mod, newdata = bathy_utm)
  bathy_utm[["SST"]] <- predict(sst_mod, newdata = bathy_utm)
  
  end_time <- Sys.time()
  print(difftime(end_time, start_time))
  
  bt_rast <- 
    dplyr::select(bathy_utm, BT) |>
    sf::st_transform(crs = "EPSG:3338") |>
    terra::rasterize(y = bathy, field = "BT")
  
  varnames(bt_rast) <- "gear_temperature"
  
  names(bt_rast) <- unique_years[jj]
  
  sst_rast <- 
    dplyr::select(bathy_utm, SST) |>
    sf::st_transform(crs = "EPSG:3338") |>
    terra::rasterize(y = bathy, field = "SST")
  
  varnames(sst_rast) <- "surface_temperature"
  
  names(sst_rast) <- unique_years[jj]
  
  if(jj == 1) {
    bt_layers <- bt_rast
    sst_layers <- sst_rast
  } else {
    bt_layers <- c(bt_layers, bt_rast)
    sst_layers <- c(sst_layers, sst_rast)
  }
  
}

# Region-specific data wrangling

if(region == "GOA") {
  
  # Eastern GOA wasn't sampled in 2001
  bt_layers[["2001"]] <- 
    terra::mask(
      bt_layers["2001"],
      esr_subareas[esr_subareas$AREA_NAME == "Eastern Gulf of Alaska", ],
      inverse = TRUE
    )
  
}

saveRDS(bt_layers, here::here("output", paste0(region, "_bt.rds")))
saveRDS(sst_layers, here::here("output", paste0(region, "_sst.rds")))

