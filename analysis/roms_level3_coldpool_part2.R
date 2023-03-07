# roms_level3_coldpool_part2.R
#
# This script is part of the post-processing suite for the Bering10K ROMS 
# hindcast simulation.  It calculates the survey-based and survey-replicated 
# cold pool indices and adds those to the existing cold pool index file 
# (created by roms_level3_coldpool_part1.m)

library(coldpool)
library(ncdf4)
library(lubridate)
# source('extended_utils.R') Functions are now included in the package

# Index file

moxdir = "~/Documents/Research/Working/mox_bumblereem/"
fname <- file.path(moxdir, "roms_for_public", "B10K-K20_CORECFS", "Level3", "B10K-K20_CORECFS_coldpool.nc")
# fname = here::here("test.nc")
ncin <- nc_open(fname)

# Read existing data and parse years we need to calculate

time <- ncvar_get(ncin,"time")
tdate <- lubridate::ymd("1900-01-01") + lubridate::days(time)

meanbtmp <- ncvar_get(ncin,"average_bottom_temp")
cpindex <- ncvar_get(ncin, "cold_pool_index")
thresh <- ncvar_get(ncin, "threshold")

nc_close(ncin)

ismiss <- meanbtmp[1,2,] > 1e36 # Don't know how to query fill value in R...

select_years <- year(tdate[ismiss])
select_years <- select_years[select_years >= 1982 & select_years != 2020]
# select_years <- c(2005, 2017, 2018, 2019)

# User-set variables

proj_crs <- coldpool:::ebs_proj_crs
cell_resolution <- 5000

# Masks: w/ and w/o NW (82+90), w/ and w/o narrowed-to-model-200m-isobath

akSEBS <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs = proj_crs)

b10_lte_200 <- sf::st_read(here::here("data", "B10K_lte200m_polygon.shp"), quiet = TRUE) %>%
  st_set_crs("+proj=longlat") %>%
  sf::st_transform(crs = sf::st_crs(proj_crs)) 

akSEBSnonw <- akSEBS$survey.strata %>%
  dplyr::filter(Stratum <= 62) %>%
  dplyr::group_by(SURVEY) %>%
  dplyr::summarise()

mymask <- list(akSEBSnonw, 
               akSEBS$survey.area,
               st_intersection(akSEBSnonw, b10_lte_200), 
               st_intersection(akSEBS$survey.area, b10_lte_200))

# Calculate indices using survey and survey-replicated data

datafile <- here::here("data", "survey_replicates_B10K-K20_CORECFS.csv")

cpname <- gsub("-", "m", sprintf("AREA_LTE_%.1f", thresh))

yrmask <- year(tdate) %in% select_years
for (ii in 1:length(mymask)) {
  
  totarea <- as.numeric(st_area(mymask[[ii]]))/1e6 # m^2 to km^2
  
  svy  <- calcindices_temp(datafile, mymask[[ii]],
                           select_years = select_years, 
                           bottomvar="gear_temperature", 
                           surfacevar=NULL, 
                           threshold=thresh)
  
  srep <- calcindices_temp(datafile, mymask[[ii]],
                           select_years = select_years, 
                           bottomvar="model_bottom_temp", 
                           surfacevar=NULL,
                           threshold=thresh)
  
  meanbtmp[ii,2,yrmask] <- svy$MEAN_GEAR_TEMPERATURE
  meanbtmp[ii,3,yrmask] <- srep$MEAN_GEAR_TEMPERATURE
    
  cpindex[,ii,2,yrmask] <- aperm(data.matrix(svy[,cpname]), c(2,1))/totarea
  cpindex[,ii,3,yrmask] <- aperm(data.matrix(srep[,cpname]), c(2,1))/totarea
}

# Write to file

ncout <- nc_open(fname, write=TRUE)

ncvar_put(ncout, "average_bottom_temp", meanbtmp)
ncvar_put(ncout, "cold_pool_index", cpindex)

hisstr <- ncatt_get(ncout, 0, "history")
if (length(select_years) == 1) {
  newhis <- paste0(date(), ": Survey and survey-replicated data added for year ", select_years)
} else {
  newhis <- paste0(date(), ": Survey and survey-replicated data added for year ", min(select_years), "-", max(select_years))
}
hisstr <- paste(newhis, hisstr$value, sep="\n")

ncatt_put(ncout, 0, "history", hisstr)

nc_close(ncout)
