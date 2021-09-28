#' Cold Pool Index (cold_pool_index)
#' 
#' Data frame containing the cold pool index, defined as the area of the eastern Bering Sea continental shelf bottom trawl survey area with bottom temperatures less than or equal to two (2) degrees celsius during AFSC/RACE/GAP summer bottom trawl surveys.
#' 
#' @format A data frame with 39 rows and 3 columns.
#' \describe{
#'      \item{YEAR}{Year}
#'      \item{COLD_POOL_AREA_KM2}{Cold Pool Index. Total area with bottom temperatures less than or equal to 2 celsius, in square kilometers}
#'      \item{LAST_UPDATE}{Date when cold pool index was last updated.}
#' }
#' @source \url{https://www.fisheries.noaa.gov/contact/groundfish-assessment-program}
"cold_pool_index"

#' Historical cold pool area calculations (cpa_pre2021)
#' 
#' Historical temperature cold pool and temperature band areas that were calculated prior to 2021 by Bob Lauth and Lyle Britt. Historical cold pool calculations were conducted in ArcGIS and used inverse distance weighting with a maximum of four nearest neighbor points for interpolation (search radius nmax = 4) and weighting function power equal to 2 (i.e., idp = 2). Default ArcGIS settings were used to generate rasters cells for interpolation (cell dimensions equal to the smaller horizontal or vertical dimension divided by 250). Protocols for including stations for interpolation varied among years but it was intended for all standard survey grid stations from the EBS survey to be included. In some years, it is possible that near-shore stations or red king crab resample stations were included in the calculations. Rasters were converted to polygon shapefiles and areas were calculated from polygons. The CRS used in ArcGIS .mxd files was North American Datum 1983 / Albers Equal Area Alaska (EPSG:3338)
#' 
#' @format A data frame with 38 rows and 9 columns:
#' \describe{
#'      \item{YEAR}{Year}
#'      \item{AVG_STRATA_WEIGHTED_BOTTEMP_STD_AREA}{Mean bottom temperature by stratum, weighted by stratum area, in square kilometers}
#'      \item{AREA_KM2_MINUS1}{Area with bottom temperatures less than -1 celsius, in square kilometers}
#'      \item{AREA_KM2_0}{Area with bottom temperatures between -1 and 0 celsius, in square kilometers}
#'      \item{AREA_KM2_1}{Area with bottom temperatures between 0 and 1 celsius, in square kilometers}
#'      \item{AREA_KM2_2}{Area with bottom temperatures between 1 and 2 celsius, in square kilometers}
#'      \item{AREA_SUM_KM2_LTE2}{Cold Pool Index. Total area with bottom temperatures less than or equal to 2 celsius, in square kilometers}
#'      \item{AREA_KM2_LTE1}{Total area with bottom temperatures less than or equal to 1 celsius, in square kilometers}
#'       \item{AREA_KM2_LTE0}{Total area with bottom temperatures less than or equal to 0 celsius, in square kilometers}
#' }
#' @source \url{https://www.fisheries.noaa.gov/contact/groundfish-assessment-program}
"cpa_pre2021"

#' CRS for eastern Bering Sea cold pool index
#' 
#' Character vector for North American Datum 1983 / Albers Equal Area Alaska (EPSG:3338).
#' 
#' @format A 1L character vector
"ebs_proj_crs"

#' Raster layers of temperature for the eastern Bering Sea
#' 
#' 5x5 km resolution raster grid of temperature in the eastern Bering Sea continental shelf survey area, from summer bottom trawl surveys conducted by AFSC/RACE/GAP.
#' 
#' @format A rasterLayer with number of layers equal to the number of survey years (1982-2021).
#' #' \describe{
#'      \item{ste_(year)_gear_temperature}{Raster layer of temperature for a given year from ordinary kriging interpolation with Stein's Matern variogram.}
#' }
"ebs_summer_temperature"