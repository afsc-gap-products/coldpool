# Update EBS and NBS temperature products

# 1. Setup project options ----

library(coldpool)
library(akgfmaps)

# 2. Set global options ----
fig_res <- 600
proj_crs <- coldpool::ebs_proj_crs
update_sysdata <- TRUE

# Setup mask to calculate mean bottom temperature from <100 m strata

ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs",
                                        set.crs = coldpool::ebs_proj_crs)
# Filepath to csv containing data to use for temperature interpolation
ebs_csv_path <- here::here("data", paste0("index_hauls_temperature_data.csv"))
nbs_ebs_csv_path <- here::here("data", paste0("ebs_nbs_temperature_full_area.csv"))

nbs_bt_years <- c(2010, 2017, 2018, 2019, 2021, 2022, 2023)
nbs_sst_years <- c(2010, 2017, 2018, 2019, 2021, 2022, 2023)
nbs_salinity_years <- c(2010, 2017, 2021, 2022, 2023)

# 3. Retrieve temperature data ----

# Update system data with new cold pool area estimates 

if(update_sysdata) {
  channel <- get_connected(schema = "AFSC")
  
  # Get temperature data and write csvs to data directory
  coldpool:::get_data(channel = channel, include_preliminary_data = "ebs")
}

# 4. Interpolate bottom and surface temperature ---- 
# Use ordinary kriging with Stein's Matern to interpolate temperature and write GeoTIFF rasters to
# output/raster/[variable].

# SEBS gear temperature
interpolation_wrapper(
  temp_data_path = ebs_csv_path,
  proj_crs = proj_crs,
  cell_resolution = c(5000, 5000), # 5x5 km grid resolution
  select_years = 2025, #1982:2025,
  interp_variable = "gear_temperature",
  select_region = "sebs",
  methods = "Ste"
) 

# SEBS surface temperature
interpolation_wrapper(
  temp_data_path = ebs_csv_path,
  proj_crs = proj_crs,
  cell_resolution = c(5000, 5000), # 5x5 km grid resolution
  select_years = 2025, #1982:2025,
  interp_variable = "surface_temperature",
  select_region = "sebs",
  methods = "Ste"
) 

# Full EBS gear temperature
interpolation_wrapper(
  temp_data_path = nbs_ebs_csv_path,
  proj_crs = proj_crs,
  cell_resolution = c(5000, 5000), # 5x5 km grid resolution
  select_years = nbs_bt_years,
  interp_variable = "gear_temperature",
  select_region = "ebs",
  methods = "Ste"
) # Full EBS+NBS

# Full EBS surface temperature
interpolation_wrapper(
  temp_data_path = nbs_ebs_csv_path,
  proj_crs = proj_crs,
  cell_resolution = c(5000, 5000), # 5x5 km grid resolution
  select_years = nbs_sst_years,
  interp_variable = "surface_temperature",
  select_region = "ebs",
  methods = "Ste"
) # Full EBS+NBS

# 5. Calculate cold pool area, mean bottom temperature, surface temperature ----
# Use surface and bottom temperature rasters to calculate cold pool area, mean bottom temperature, 
# and mean surface temperature.
# Writes GeoTIFF rasters to  - /output/raster/[METHOD]_[YEAR]_gear_temperature.tif

# Calculate cold pool area and mean bottom temperature from SEBS rasters
bottom_temp_files <- 
  list.files(here::here("output", "raster", "sebs", "gear_temperature"), 
             full.names = TRUE,
             pattern = "ste_")

bt_df <- 
  data.frame(
    YEAR = numeric(length = length(bottom_temp_files)),
    AREA_LTE2_KM2 = numeric(length = length(bottom_temp_files)),
    AREA_LTE1_KM2 = numeric(length = length(bottom_temp_files)),
    AREA_LTE0_KM2 = numeric(length = length(bottom_temp_files)),
    AREA_LTEMINUS1_KM2 = numeric(length = length(bottom_temp_files)),
    MEAN_GEAR_TEMPERATURE = numeric(length = length(bottom_temp_files)),
    MEAN_BT_LT100M = numeric(length = length(bottom_temp_files))
  )

lt100_strata <- 
  ebs_layers$survey.strata |>
  dplyr::filter(STRATUM %in% c(10, 20, 31, 32, 41, 42, 43)) |>
  dplyr::group_by(SURVEY_DEFINITION_ID) |>
  dplyr::summarise()


for(ii in 1:length(bottom_temp_files)) {
  
  bt_raster <- terra::rast(bottom_temp_files[ii])
  
  bt_df$YEAR[ii] <- 
    as.numeric(gsub("[^0-9.-]", "", names(bt_raster))) # Extract year
  
  bt_df$AREA_LTE2_KM2[ii] <- 
    bt_raster |> 
    cpa_from_raster(raster_units = "m", temperature_threshold = 2)
  
  bt_df$AREA_LTE1_KM2[ii] <- 
    bt_raster |> 
    cpa_from_raster(raster_units = "m", temperature_threshold = 1)
  
  bt_df$AREA_LTE0_KM2[ii] <- 
    bt_raster |> 
    cpa_from_raster(raster_units = "m", temperature_threshold = 0)
  
  bt_df$AREA_LTEMINUS1_KM2[ii] <- 
    bt_raster |> 
    cpa_from_raster(raster_units = "m", temperature_threshold = -1)
  
  bt_df$MEAN_GEAR_TEMPERATURE[ii] <- 
    mean(
      terra::values(bt_raster), 
      na.rm = TRUE
      )
  
  lt100_temp <- 
    terra::mask(
      bt_raster, 
      lt100_strata,
      touches = FALSE
    )
  
  bt_df$MEAN_BT_LT100M[ii] <- mean(terra::values(lt100_temp), na.rm = TRUE) 
  
}

# Calculate mean surface temperature
surface_temp_files <- 
  list.files(
    here::here("output", "raster", "sebs", "surface_temperature"), 
    full.names = TRUE,
    pattern = "ste_"
  )

sst_df <- 
  data.frame(
    YEAR = numeric(length = length(bottom_temp_files)),
    MEAN_SURFACE_TEMPERATURE = numeric(length = length(surface_temp_files))
  )

for(ii in 1:length(surface_temp_files)) {
  
  sst_raster <- 
    terra::rast(surface_temp_files[ii])
  
  sst_df$YEAR[ii] <- 
    as.numeric(gsub("[^0-9.-]", "", names(sst_raster)))  # Extract year
  
  sst_df$MEAN_SURFACE_TEMPERATURE[ii] <- 
    mean(
      terra::values(
        sst_raster), 
      na.rm = TRUE
      )
  
}

cold_pool_index <- dplyr::inner_join(bt_df, sst_df)

# Retrieve NBS survey area for mask
nbs_area <- 
  akgfmaps::get_base_layers(
    select.region = "ebs", 
    set.crs = coldpool::ebs_proj_crs)$survey.area |>
  dplyr::filter(SURVEY_DEFINITION_ID == 143)

nbs_ebs_bt_files <- 
  list.files(
    here::here("output", "raster", "ebs", "gear_temperature"), 
    full.names = TRUE,
    pattern = "ste_"
  )

nbs_ebs_sst_files <- 
  list.files(
    here::here("output", "raster", "ebs", "surface_temperature"), 
    full.names = TRUE,
    pattern = "ste_"
  )

nbs_mean_temperature <- 
  data.frame(
    YEAR = numeric(length = length(nbs_ebs_bt_files)),
    MEAN_GEAR_TEMPERATURE = numeric(length = length(nbs_ebs_bt_files)),
    MEAN_SURFACE_TEMPERATURE = numeric(length = length(nbs_ebs_bt_files))
  )

for(ii in 1:length(nbs_ebs_bt_files)) {
  
  nbs_bt_raster <- 
    nbs_ebs_bt_files[ii] |>
    terra::rast() |>
    terra::mask(nbs_area, touches = FALSE)
  
  nbs_mean_temperature$YEAR[ii] <- 
    as.numeric(
      gsub(pattern = "[^0-9.-]", 
           replacement = "", 
           x = names(nbs_bt_raster))) # Extract year
  
  nbs_mean_temperature$MEAN_GEAR_TEMPERATURE[ii] <- 
    nbs_bt_raster |>
    terra::values() |>
    mean(na.rm = TRUE)
  
  nbs_mean_temperature$AREA_LTE2_KM2[ii] <- 
    nbs_bt_raster |> 
    cpa_from_raster(raster_units = "m", temperature_threshold = 2)
  
  nbs_mean_temperature$AREA_LTE1_KM2[ii] <- 
    nbs_bt_raster |> 
    cpa_from_raster(raster_units = "m", temperature_threshold = 1)
  
  nbs_mean_temperature$AREA_LTE0_KM2[ii] <- 
    nbs_bt_raster |> 
    cpa_from_raster(raster_units = "m", temperature_threshold = 0)
  
  nbs_mean_temperature$AREA_LTEMINUS1_KM2[ii] <- 
    nbs_bt_raster |> 
    cpa_from_raster(raster_units = "m", temperature_threshold = -1)
  
  
  # Don't calculate SST if NBS data haven't been finalized
  if(file.exists(nbs_ebs_sst_files[ii])) {
    
    nbs_sst_raster <- 
      terra::rast(nbs_ebs_sst_files[ii]) |>
      terra::mask(nbs_area, touches = FALSE)
    
    nbs_mean_temperature$MEAN_SURFACE_TEMPERATURE[ii] <- 
      mean(terra::values(nbs_sst_raster), na.rm = TRUE)
    
  } else {
    nbs_mean_temperature$MEAN_SURFACE_TEMPERATURE[ii] <- NA
  }
  
}

nbs_mean_temperature <- 
  nbs_mean_temperature |>
  dplyr::filter(YEAR != 2018)

# Summary statistics for write-up
cpa_previous_year <- 
  tail(
    cold_pool_index$AREA_LTE2_KM2, 2
    )[1]

cpa_this_year <- 
  tail(
    cold_pool_index$AREA_LTE2_KM2, 1
    )

(cpa_previous_year - cpa_this_year) /cpa_previous_year


lte1_previous_year <- tail(cold_pool_index$AREA_LTE1_KM2,2)[1]

lte1_this_year <- tail(cold_pool_index$AREA_LTE1_KM2,1)

(lte1_previous_year - lte1_this_year) / lte1_previous_year


lte0_previous_year <- tail(cold_pool_index$AREA_LTE0_KM2,2)[1]

lte0_this_year <- tail(cold_pool_index$AREA_LTE0_KM2,1)

(lte0_previous_year - lte0_this_year) / lte0_previous_year


lteminus1_previous_year <- tail(cold_pool_index$AREA_LTEMINUS1_KM2,2)[1]

lteminus1_this_year <- tail(cold_pool_index$AREA_LTEMINUS1_KM2,1)

(lteminus1_previous_year - lteminus1_this_year) / lteminus1_previous_year

# 6. Interpolate salinity ----

# Functions for reading in salinity data
make_pre_2020_summary <- function() {
  
  nc_files <- list.files(
    system.file(
      "extdata", "nc", package = "gapctd"
    ), 
    pattern = "Bottom_Trawl_Survey_CTD", 
    full.names = TRUE
  )
  
  bottom_dat <- data.frame()
  
  for(ii in 1:length(nc_files)) {
    nc <- RNetCDF::open.nc(con = nc_files[ii])
    
    if(ii > 3) {
      start_date <- 
        as.POSIXct(
          as.Date(
            gsub(
              pattern = "Since ", 
              replacement = "", 
              x = RNetCDF::att.get.nc(
                ncfile = nc, 
                variable = "DATE1", 
                attribute = "long_name")
            ),
            format = "%d-%B-%Y")
        )
      
      new_dat <- 
        data.frame(LATITUDE = RNetCDF::var.get.nc(ncfile = nc, variable = "LATD1"),
                   LONGITUDE = RNetCDF::var.get.nc(ncfile = nc, variable = "LOND1"),
                   BOTTOM_SAL_PSU = 
                     apply(
                       X = RNetCDF::var.get.nc(
                         ncfile = nc, 
                         variable = "SAL2"), 
                       MARGIN = 1, 
                       FUN = function(x) x[max(which(!is.na(x)))]
                     ),
                   BOTTOM_TEMP_C = 
                     apply(
                       X = RNetCDF::var.get.nc(
                         ncfile = nc, variable = "TEMP2"), 
                       MARGIN = 1, 
                       FUN = function(x) x[max(which(!is.na(x)))]
                     ),
                   YEAR = 
                     floor(
                       as.numeric(
                         RNetCDF::var.get.nc(
                           ncfile = nc, 
                           variable = "CRUISE1")
                       )/100),
                   STATIONID = 
                     gsub(
                       pattern = " ", 
                       replacement = "", 
                       x = RNetCDF::var.get.nc(
                         ncfile = nc, 
                         variable = "STN_NM1")
                     ),
                   DATE = 
                     as.character(
                       start_date + (RNetCDF::var.get.nc(ncfile = nc, variable = "DATE1")*24*3600))
        )
    } else {
      start_date <- 
        as.POSIXct(
          as.Date(
            gsub(
              pattern = "Since ", 
              replacement = "", 
              x = RNetCDF::att.get.nc(
                ncfile = nc, 
                variable = "DATE", 
                attribute = "long_name")
            ),
            format = "%d-%B-%Y")
        )
      
      
      new_dat <- 
        data.frame(
          LATITUDE = 
            RNetCDF::var.get.nc(
              ncfile = nc, 
              variable = "LAT"
            ),
          LONGITUDE = 
            RNetCDF::var.get.nc(
              ncfile = nc, 
              variable = "LON"
            ),
          BOTTOM_SAL_PSU = 
            apply(
              X = RNetCDF::var.get.nc(
                ncfile = nc, 
                variable = "SAL"
              ), 
              MARGIN = 2, 
              FUN = function(x) x[max(which(!is.na(x)))]
            ),
          BOTTOM_TEMP_C = 
            apply(
              X = RNetCDF::var.get.nc(
                ncfile = nc, 
                variable = "TEMP"
              ), 
              MARGIN = 2, 
              FUN = function(x) x[max(which(!is.na(x)))]
            ),
          YEAR = 
            floor(
              as.numeric(
                RNetCDF::var.get.nc(
                  ncfile = nc, 
                  variable = "CRUISE")
              )/100
            ),
          STATIONID = 
            gsub(
              pattern = " ", 
              replacement = "", 
              x = RNetCDF::var.get.nc(
                ncfile = nc, 
                variable = "STN_NM")
            ),
          DATE = 
            as.character(
              start_date + (RNetCDF::var.get.nc(ncfile = nc, variable = "DATE")*24*3600)
            )
        )
    }
    
    bottom_dat <- 
      dplyr::bind_rows(
        bottom_dat, 
        new_dat
      )
    
    RNetCDF::close.nc(nc)
  }
  
  return(bottom_dat)
}

make_post_2020_summary <- function() {
  
  nc_files <- 
    list.files(
      here::here("assets"), 
      pattern = "_EBS.nc", 
      full.names = TRUE
    )
  
  bottom_dat <- data.frame()
  
  for(jj in 1:length(nc_files)) {
    
    nc <- RNetCDF::open.nc(con = nc_files[jj])
    
    new_dat <- 
      data.frame(
        LATITUDE = 
          RNetCDF::var.get.nc(
            ncfile = nc, 
            variable = "latitude"
          ),
        LONGITUDE = 
          RNetCDF::var.get.nc(
            ncfile = nc, 
            variable = "longitude"
          ),
        BOTTOM_SAL_PSU = 
          apply(
            X = RNetCDF::var.get.nc(
              ncfile = nc, 
              variable = "sea_floor_practical_salinity"
            ),
            MARGIN = 1,
            FUN = function(x) x[max(which(!is.na(x)))]
          ),
        BOTTOM_TEMP_C = 
          apply(
            X = RNetCDF::var.get.nc(
              ncfile = nc, 
              variable = "sea_floor_temperature"), 
            MARGIN = 1, 
            FUN = function(x) x[max(which(!is.na(x)))]
          ),
        STATIONID = 
          RNetCDF::var.get.nc(
            ncfile = nc, 
            variable = "stationid"
          ),
        DATE = 
          RNetCDF::var.get.nc(
            ncfile = nc, 
            variable = "time")
      ) |>
      dplyr::mutate(DATE = as.POSIXct(DATE, tz = "UTC")) |>
      dplyr::mutate(YEAR = lubridate::year(DATE))              
    
    
    bottom_dat <- 
      dplyr::bind_rows(
        bottom_dat, 
        new_dat
      )
    
    RNetCDF::close.nc(nc)
    
  }
  
  return(bottom_dat)
  
}

# Retrieve haul data and prep data
haul_dat <- 
  RODBC::sqlQuery(
  channel = channel, 
  query = 
    "SELECT 
      VESSEL, 
      CRUISE,
      HAUL,
      START_TIME,
      HAUL_TYPE,
      PERFORMANCE,
      REGION,
      STATIONID
    FROM 
      RACEBASE.HAUL 
    WHERE
      CRUISE > 200800 
      AND HAUL_TYPE = 3 
      AND PERFORMANCE = 0 
      AND REGION = 'BS'")

names(haul_dat) <- tolower(names(haul_dat))

haul_dat$year <- floor(haul_dat$cruise/100)

pre_2020_ctd_df <- make_pre_2020_summary()
names(pre_2020_ctd_df) <- tolower(names(pre_2020_ctd_df))

pre_2020_ctd_df <- 
  dplyr::inner_join(
    pre_2020_ctd_df, 
    haul_dat
    ) |>
  dplyr::mutate(date = as.POSIXct(date)) |>
  dplyr::filter(lubridate::yday(date) == lubridate::yday(start_time))

pre_2020_ctd_df <- 
  pre_2020_ctd_df |>
  dplyr::select(year, stationid, date) |>
  dplyr::group_by(year, stationid) |>
  dplyr::summarize(date = max(date)) |>
  dplyr::inner_join(pre_2020_ctd_df)

post_2020_ctd_df <- 
  make_post_2020_summary()

names(post_2020_ctd_df ) <- 
  tolower(names(post_2020_ctd_df))

post_2020_ctd_df <- 
  post_2020_ctd_df |>
  dplyr::select(year, stationid, date) |>
  dplyr::group_by(year, stationid) |>
  dplyr::summarize(date = max(date)) |>
  dplyr::inner_join(post_2020_ctd_df)

bottom_temp_sal <- 
  dplyr::bind_rows(
    pre_2020_ctd_df, 
    post_2020_ctd_df
    )

sal_years <- unique(bottom_temp_sal$year)

# Interpolation
for(kk in sal_years) {
  
  coldpool::interpolate_variable(
    dat = bottom_temp_sal |>
      dplyr::filter(year == kk),
    dat.year = kk,
    var.col = "bottom_sal_psu",
    lat.col = "latitude",
    lon.col = "longitude",
    interpolation.crs = "EPSG:3338",
    cell.resolution = c(5000, 5000),
    select.region = "sebs",
    methods = "ste"
  )
  
}

ebs_bottom_salinity <- 
  coldpool::make_raster_stack(
    file_path = here::here("output", "raster", "sebs", "bottom_sal_psu"),
    file_name_contains = "ste_",
    wrap = TRUE
  )

for(kk in nbs_salinity_years) {
  
  coldpool::interpolate_variable(
    dat = bottom_temp_sal |>
      dplyr::filter(year == kk),
    dat.year = kk,
    var.col = "bottom_sal_psu",
    lat.col = "latitude",
    lon.col = "longitude",
    interpolation.crs = "EPSG:3338",
    cell.resolution = c(5000, 5000),
    select.region = "ebs",
    methods = "ste"
  )
  
}

nbs_ebs_bottom_salinity <- 
  coldpool::make_raster_stack(
    file_path = here::here("output", "raster", "ebs", "bottom_sal_psu"),
    file_name_contains = "ste_",
    wrap = TRUE
  )

# 7. Calculate salinity summary statistics and append to CPI and NBS data frames ----
ebs_sal_df <- 
  ebs_bottom_salinity |>
  terra::unwrap() |>
  terra::global(
    fun = mean, 
    na.rm = TRUE) |>
  as.data.frame()

names(ebs_sal_df) <- "MEAN_GEAR_SALINITY"
ebs_sal_df$YEAR <- as.numeric(rownames(ebs_sal_df))

cold_pool_index <- 
  dplyr::full_join(
    cold_pool_index, 
    ebs_sal_df, 
    by = "YEAR"
    )

nbs_ebs_sal_df <- 
  nbs_ebs_bottom_salinity |>
  terra::unwrap() |>
  terra::mask(
    mask = nbs_area
  ) |>
  terra::global(
    fun = mean, 
    na.rm = TRUE) |>
  as.data.frame()

names(nbs_ebs_sal_df) <- "MEAN_GEAR_SALINITY"
nbs_ebs_sal_df$YEAR <- as.numeric(rownames(nbs_ebs_sal_df))

nbs_mean_temperature <- 
  dplyr::full_join(
    nbs_mean_temperature,
    nbs_ebs_sal_df,
    by = "YEAR"
  )

# 8. Add data to built-in data sets for lazy loading ----

# Need to update documentation and build/install after running this code.

# Make surface temperature raster stack (multiplying by 1 resets the source)
ebs_surface_temperature <- 
  coldpool::make_raster_stack(
    file_path = here::here("output", "raster", "sebs", "surface_temperature"),
    file_name_contains = "ste_",
    file_type = ".tif",
    wrap = TRUE
  )

# Make bottom temperature raster stack (multiplying by 1 resets the source)
ebs_bottom_temperature <- 
  coldpool::make_raster_stack(
    file_path = here::here("output", "raster", "sebs", "gear_temperature"),
    file_name_contains = "ste_",
    file_type = ".tif",
    wrap = TRUE
  )

# Make surface temperature raster stack (multiplying by 1 resets the source)
nbs_ebs_surface_temperature <- 
  coldpool::make_raster_stack(
    file_path = here::here("output", "raster", "ebs", "surface_temperature"),
    file_name_contains = "ste_",
    file_type = ".tif",
    wrap = TRUE
  )

# Make bottom temperature raster stack (multiplying by 1 resets the source)
nbs_ebs_bottom_temperature <- 
  coldpool::make_raster_stack(
    file_path = here::here("output", "raster", "ebs", "gear_temperature"),
    file_name_contains = "ste_",
    file_type = ".tif",
    wrap = TRUE
  )

cold_pool_index$LAST_UPDATE <- Sys.Date()
nbs_mean_temperature$LAST_UPDATE <- Sys.Date()

cpa_pre2021 <- read.csv(file = here::here("inst", "extdata", "old_method_cpa_temperature_2021.csv"))
ebs_proj_crs <- coldpool::ebs_proj_crs

usethis::use_data(cpa_pre2021, overwrite = TRUE)
usethis::use_data(ebs_proj_crs, overwrite = TRUE)
usethis::use_data(cold_pool_index, overwrite = TRUE)
usethis::use_data(nbs_mean_temperature, overwrite = TRUE)
usethis::use_data(ebs_bottom_temperature, overwrite = TRUE)
usethis::use_data(ebs_surface_temperature, overwrite = TRUE)
usethis::use_data(nbs_ebs_bottom_temperature, overwrite = TRUE)
usethis::use_data(nbs_ebs_surface_temperature, overwrite = TRUE)
usethis::use_data(ebs_bottom_salinity, overwrite = TRUE)
usethis::use_data(nbs_ebs_bottom_salinity, overwrite = TRUE) 

