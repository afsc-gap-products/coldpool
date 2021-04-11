# Load libraries ----
library(akgfmaps) # GitHub: sean-rohan-NOAA/akgfmaps
library(getPass)
library(ggthemes)
library(here)
library(lubridate)
library(metR)
library(raster)
library(RODBC)
library(stringr)
library(tidyverse)
library(TLUtilities) # GitHub: sean-rohan-NOAA/TLUtilities
library(viridis)

# Functions ----
source(here::here("R", "get_data.R"))
source(here::here("R", "loocv_gear_temp.R"))
source(here::here("R", "interpolate_gear_temp.R"))
source(here::here("R", "plot_loocv_rmse.R"))


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

# Themes ----

theme_multi_map <- function() {
  return(theme_base() %+replace%
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.grid = element_blank(),
        plot.title = element_text(size = 9, margin = margin(t = 0, r = 0, b = 5, l = 0, unit = "pt")),
        axis.title = element_blank(),
        axis.text = element_text(size = 9),
        legend.position = "none",
        plot.background = element_rect(fill = NA, color = NA)))
}
