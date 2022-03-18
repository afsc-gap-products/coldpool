#' Cold Pool Index and SEBS Mean Temperature (cold_pool_index)
#' 
#' Data frame containing mean gear temperature (i.e., bottom temperature), mean surface temperatures, and areas of temperatures less than or equal to 2, 1, 0, and -1 degree celcius. The Cold Pool Index (i.e., cold pool area) is the area of the eastern Bering Sea continental shelf bottom trawl survey area with bottom temperatures less than or equal to two (2) degrees celsius during AFSC/RACE/GAP summer bottom trawl surveys. In this data set, the Cold Pool Index has the column name AREA_LTE2_KM2.
#' 
#' @format A data frame with 39 rows and 8 columns.
#' \describe{
#'      \item{YEAR}{Year}
#'      \item{AREA_LTE2_KM2}{Cold Pool Index. Total area with bottom temperatures less than or equal to 2 celsius, in square kilometers}
#'      \item{AREA_LTE1_KM2}{Total area with bottom temperatures less than or equal to 1 celsius, in square kilometers}
#'      \item{AREA_LTE0_KM2}{Total area with bottom temperatures less than or equal to 0 celsius, in square kilometers}
#'      \item{AREA_LTEMINUS1_KM2}{Total area with bottom temperatures less than or equal to -1 celsius, in square kilometers}
#'      \item{MEAN_GEAR_TEMPERATURE}{Mean gear temperature (i.e. bottom temperature) in the survey area.}
#'      \item{MEAN_SURFACE_TEMPERATURE}{Mean sea surface temperature in the survey area.}
#'      \item{LAST_UPDATE}{Date when cold pool index and temperature rasters were last updated.}
#' }
#' @source \url{https://www.fisheries.noaa.gov/contact/groundfish-assessment-program}
#' @export
"cold_pool_index"

#' Northern Bering Sea Mean Temperature (nbs_mean_temperature)
#' 
#' Data frame containing average bottom temperature and surface temperature for the NBS survey area. Does not include the unplanned 2018 northern extension because the entire survey area was not sampled.
#' 
#' @format A data frame with 4 rows and 4 columns.
#' \describe{
#'      \item{YEAR}{Year}
#'      \item{MEAN_GEAR_TEMPERATURE}{Mean gear temperature (i.e. bottom temperature) in the survey area.}
#'      \item{MEAN_SURFACE_TEMPERATURE}{Mean sea surface temperature in the survey area.}
#'      \item{LAST_UPDATE}{Date when cold pool index and temperature rasters were last updated.}
#' }
#' @source \url{https://www.fisheries.noaa.gov/contact/groundfish-assessment-program}
#' @export
"nbs_mean_temperature"

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
#' @export
"ebs_proj_crs"

#' Rasters of summer bottom temperature in the eastern Bering Sea at 5-km resolution
#' 
#' Summer bottom temperatures in the eastern Bering Sea continental shelf survey area, calculated from interpolation of temperature data from summer bottom trawl surveys conducted by AFSC/RACE/GAP. To load this data set, it is necessary to first load the coldpool package or raster and sp packages. Raster layers were interpolated using ordinary kriging with Matern covariance \doi{10.1007/978-1-4612-1494-6}{(Stein, 1999)}.
#' 
#' @format A RasterStack class with 39 layers, one for every year from 1982-2021, except for 2020 (due to survey cancellation).
#' #' \describe{
#'      \item{ste_(year)_gear_temperature}{Raster layer of temperature for a given year}
#' }
#' @source \url{https://www.fisheries.noaa.gov/contact/groundfish-assessment-program}
#' @export
"ebs_bottom_temperature"

#' Rasters of summer sea surface temperature in the eastern Bering Sea at 5-km resolution
#' 
#' Summer surface temperatures in the eastern Bering Sea continental shelf survey area, calculated from interpolation of temperature data from summer bottom trawl surveys conducted by AFSC/RACE/GAP. To load ethis data set, it is necessary to first load the coldpool package or raster and sp packages. Raster layers were interpolated using ordinary kriging with Matern covariance \doi{10.1007/978-1-4612-1494-6}{(Stein, 1999)}.
#' 
#' @format A RasterStack class with 39 layers, one for every year from 1982-2021, except for 2020 (due to survey cancellation).
#' #' \describe{
#'      \item{ste_(year)_gear_temperature}{Raster layer of temperature for a given year}
#' }
#' @source \url{https://www.fisheries.noaa.gov/contact/groundfish-assessment-program}
#' @export
"ebs_surface_temperature"

#' Rasters of summer bottom temperature in the eastern and northern Bering Sea at 5-km resolution
#' 
#' Summer bottom temperatures in the full eastern Bering Sea survey area, calculated from interpolation of temperature data from summer bottom trawl surveys conducted by AFSC/RACE/GAP. To load this data set, it is necessary to first load the coldpool package or raster and sp packages. Raster layers were interpolated using ordinary kriging with Matern covariance \doi{10.1007/978-1-4612-1494-6}{(Stein, 1999)}.
#' 
#' @format A RasterStack class with five layers, one for every year with an NBS survey, including the unplanned 2018 partial northern extention.
#' #' \describe{
#'      \item{ste_(year)_gear_temperature}{Raster layer of temperature for a given year}
#' }
#' @source \url{https://www.fisheries.noaa.gov/contact/groundfish-assessment-program}
#' @export
"nbs_ebs_bottom_temperature"

#' Rasters of summer bottom temperature in the eastern and northern Bering Sea at 5-km resolution
#' 
#' Summer bottom temperatures in the full eastern Bering Sea survey area, calculated from interpolation of temperature data from summer bottom trawl surveys conducted by AFSC/RACE/GAP. To load ethis data set, it is necessary to first load the coldpool package or raster and sp packages. Raster layers were interpolated using ordinary kriging with Matern covariance \doi{10.1007/978-1-4612-1494-6}{(Stein, 1999)}.
#' 
#' @format A RasterStack class with five layers, one for every year with an NBS survey, including the unplanned 2018 partial northern extention.
#' #' \describe{
#'      \item{ste_(year)_gear_temperature}{Raster layer of temperature for a given year}
#' }
#' @source \url{https://www.fisheries.noaa.gov/contact/groundfish-assessment-program}
#' @export
"nbs_ebs_surface_temperature"