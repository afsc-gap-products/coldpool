# Load libraries ----
library(akgfmaps) # GitHub: sean-rohan-NOAA/akgfmaps
library(getPass)
library(ggthemes)
library(here)
library(raster)
library(RODBC)
library(tidyverse)
library(TLUtilities) # GitHub: sean-rohan-NOAA/TLUtilities

# Functions ----
# source(here::here("R", "calculate_cold_pool_area.R"))
# source(here::here("R", "cpa_from_raster.R"))
source(here::here("R", "loocv_gear_temp.R"))
source(here::here("R", "interpolate_gear_temp.R"))
source(here::here("R", "plot_loocv_rmse.R"))
# source(here::here("R", "rasterize_and_mask.R"))

# Connect ----
get_connected <- function(schema = 'AFSC'){
  (echo = FALSE)
  username <- getPass(msg = "Enter your ORACLE Username: ")
  password <- getPass(msg = "Enter your ORACLE Password: ")
  channel  <- RODBC::odbcConnect(paste(schema),
                                 paste(username),
                                 paste(password),
                                 believeNRows=FALSE)
}

# Plotting ----
fig_res <- 300
