# Introduction
_Sean Rohan and Lewis Barnett_
<br>Groundfish Assessment Program, Resource Assessment and Conservation Engineering Division, Alaska Fisheries Science Center, National Marine Fisheries Service, NOAA

This repository contains an R package that is used to calculate the *cold pool index*, mean sea surface temperature, and mean bottom temperature using temperature data collected during bottom trawl surveys of the eastern Bering Sea conducted by NOAA/AFSC/RACE's Groundfish Assessment Program. The cold pool index is defined as the area of the NOAA/AFSC/RACE eastern Bering Sea bottom trawl survey footprint with bottom temperatures less than or equal to 2° Celsius, in square kilometers. This package includes temperature products (mean temperatures, cold pool area, interpolated temperature raster) that are updated on an annual basis following the eastern Bering Sea shelf bottom trawl survey. Temperature trends are described below in the section titled [Cold pool area and temperature trends](https://github.com/afsc-gap-products/coldpool#cold-pool-area-and-temperature-trends). 

The most recent version of this package was developed and tested using R 4.1.1.

# Installation

1. Install the [akgfmaps package](https://github.com/sean-rohan-NOAA/akgfmaps) from GitHub prior to installing coldpool, as follows:
```{r}
devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
```

If you encounter problems installing the akgfmaps package, please refer to the akgfmaps GitHub repository.

2. Install the coldpool package using the following code:
```{r}
devtools::install_github("afsc-gap-products/coldpool")
```

# Usage

## Accessing datasets using the package
Users can access temperature products directly from datasets that are built into the package. For example, after installing the package, users can access a data frame containing cold pool area (area with temperature less than or equal to 2°C), area of other isotherms (less than or equal to 1°C, 0°C, -1°C), mean bottom temperature, and mean surface temperature for the EBS, using:
```{r}
coldpool:::cold_pool_index
```

Documentation for the dataset can be accessed using:
```{r}
?coldpool:::cold_pool_index
```

## Accessing datasets without installing the package
Users can access temperature products in an R data format (.rda) without installing the package. To do so, download the [sysdata.rda](./R/sysdata.rda) file in the R directory of the coldpool repository. The data set can then be loaded in R after installing and loading the [raster](https://cran.r-project.org/web/packages/raster/index.html) package (version >= 3.2-1), as follows:

```{r}
# Load raster package and data
library(raster)
load("[local_path]\\sysdata.rda")

# View loaded cold pool index data frame
cold_pool_index
```

## Datasets in the package

<dl>
<dt><b>cold_pool_index</b></dt>
<dd><p>Data frame containing the total area of EBS waters with bottom temperatures less than or equal to 2, 1, 0, and -1 °C, mean bottom temperatures, and mean surface temperature during the EBS survey for 1982-2021 (excluding 2020 due to cancelled survey).</p></dd>
<dt><b>nbs_mean_bottom_temperature</b></dt>
<dd><p>Data frame of mean bottom temperature in the NBS during years with a full EBS+NBS standardized survey (2010, 2017, 2019, 2021).</p></dd>
<dt><b>ebs_bottom_temperature</b></dt>
<dd><p>Interpolated rasters of bottom temperature for the EBS survey area from 1982-2021 (excluding 2020 due to cancelled survey).</p></dd>
<dt><b>ebs_surface_temperature</b></dt> 
<dd><p>Interpolated rasters of sea surface temperature for the EBS survey area from 1982-2021 (excluding 2020 due to cancelled survey).</p></dd>
<dt><b>nbs_ebs_bottom_temperature</b></dt> 
<dd><p>Interpolated rasters of bottom temperature for the full EBS and NBS survey area for years with a full EBS+NBS standardized survey (2010, 2017, 2019, 2021).</p></dd>
<dt><b>nbs_ebs_surface_temperature</b></dt> 
<dd><p>Interpolated rasters of sea surface temperature for the full EBS and NBS survey area for years with a full EBS+NBS standardized survey (2010, 2017, 2019, 2021).</p></dd>
<dt><b>cpa_pre2021</b></dt>
<dd><p>Data frame of cold pool area and mean temperatures calculated using the interpolation method used prior to 2021.</p></dd>
</dl>

## Caveat emptor

The temperature data products in this package are an annual snapshot of temperatures during summer bottom trawl surveys. Combined with biological data collected during bottom trawl surveys, these temperature data can provide a simultaneous characterization of thermal habitat and demersal fauna distribution and abundance in the eastern Bering Sea. However, these temperature data products are not adjusted to account for seasonal heating so they do not provide a snapshot of temperature at a specific point in time. Users who are interested in spatially integrated or spatially resolved estimates of temperature at specific points in time that do account for seasonal heating may want to consider using temperature predictions from the [Bering10K BEST-NPZ model](https://github.com/beringnpz/roms-bering-sea) [(Kearney et al., 2020)](http://www.doi.org/10.5194/gmd-13-597-2020), [satellite-derived sea surface temperature](https://github.com/jordanwatson/aksst) products, or alternative oceanographic sampling data, such as those collected by NOAA's [EcoFOCI](https://www.ecofoci.noaa.gov/) program.

# Methods

## Data collection

Temperature data have been collected annually during AFSC's standardized summer bottom trawl surveys of the eastern Bering Sea continental shelf (EBS shelf) and northern Bering Sea (BS). The EBS shelf survey has been conducted annually since 1982 (except for 2020) and the NBS survey was conducted in 2010, 2017, 2019, and 2021. In the eastern Bering Sea, surveys are conducted from late May or early June through late July to early August and the northern Bering Sea survey is conducted immediately after the EBS shelf survey (July-August). The EBS shelf survey samples 376 index stations and the NBS survey samples 143 index stations per year, although the survey footprint and number of stations has changed over time (e.g. EBS NW strata added in 1987). The EBS shelf survey progresses from the nearshore waters inside Bristol Bay to the outer continental shelf in the NW portion of southeastern Bering Sea (Figure 1). The NBS survey starts offshore where the EBS shelf survey ends, then progresses northward towards the Bering Strait and Norton Sound, before heading south towards Nunivak Island.

![Map of eastern Bering Sea and northern Bering Sea survey areas showing the EBS standard, EBS NW, and NBS survey strata.](./plots/ebs_nbs_survey_area.png)
 <font size="2"><i>Figure 1. Map of eastern Bering Sea (EBS) shelf and northern Bering Sea (NBS) shelf survey areas and station grid, including EBS standard (sampled since 1982) and northwest (sampled since 1987) subareas. Thick black lines denote survey boundaries and fill color denotes the average day of year when stations are sampled by the survey.</i></font>

Temperature data are collected at every survey station using temperature sensors attached to the bottom trawl survey gear. The equipment used to collect temperature data has changed over time, as described in [Buckley et al. (2009)](https://repository.library.noaa.gov/view/noaa/3655).

## Interpolation and analysis

Raster surfaces and temperature products are produced by interpolating temperature data collected during surveys using ordinary kriging with Stein’s parameterization of the Matérn semivariogram. Only data from hauls with 'good' performance are included in temperature calculations. Statistics that summarize temperature patterns are produced from raster surfaces: areas of isotherms in the EBS shelf survey area (2°C, 1°C, 0°C, -1°C), mean bottom temperature over the EBS shelf and NBS (i.e., gear temperature), and sea surface temperature over the EBS shelf and NBS. For the EBS shelf survey area, temperature data used to calculate summary statistics do not include NBS data because of a warm bias in NBS survey years that is caused by the southern portion of the nearshore domain of the NBS being sampled last. The interpolation region for the EBS shelf includes both the standard area (sampled from 1982-present) and the NW strata (sampled from 1987-present) for all years (Figure 1). The NBS interpolation only includes data from years with a full NBS survey and uses data from the EBS Standard, EBS NW, and NBS survey areas.

## Data product and package updates

Temperature data products in the coldpool package are updated on an annual basis via new package releases after the conclusion of summer bottom trawl surveys.


# Cold pool area and temperature trends
*Updated: November 2021*

Cold pool area and temperature trends are reported in the annual [Ecosystem Status Reports](https://www.fisheries.noaa.gov/alaska/ecosystems/ecosystem-status-reports-gulf-alaska-bering-sea-and-aleutian-islands) for the eastern Bering Sea and ecosystem and socioeconomic profiles for EBS stocks. Temperature products are also used as covariates in some [stock assessment](https://www.fisheries.noaa.gov/alaska/population-assessments/north-pacific-groundfish-stock-assessments-and-fishery-evaluation) models or their inputs (e.g. abundance indices).

![Cold pool area from 1982-2021, based on interpolated survey bottom temperatures](./plots/coldpool_with_area.png)
 <font size="2"><i>Figure 2. Cold pool extent in the eastern Bering Sea from 2002–2021, showing areas with bottom temperatures ≤ 2°C, ≤ 1°C, ≤ 0°C, and ≤ -1°C (upper panels), and proportion of the southeastern Bering Sea survey area with bottom temperatures ≤ 2°C, ≤ 1°C, ≤ 0°C, and ≤ -1°C (lower panel). Solid black lines in the interior of the surface represent the 50m and 100m isobaths.</i></font>

The cold pool extent has increased since 2018, yet the 2021 extent (58,975 km2) was the fourth lowest on record and remains one to two standard deviations below the grand mean of the time series (Figure 2). Estimates from 2018 and 2019 were the lowest on record, followed by 2003, which was only slightly lower than the 2021 estimate. In general, the extent of isotherms at all thresholds ≤ 1°C were similar, if slightly greater than prior record lows (Figure 2). As is typical when the extent is small, the cold pool in 2021 was restricted to the northern edge of the eastern Bering Sea shelf (Figure 2). 

![Mean bottom temperature in the eastern Bering Sea during 2021, based on interpolated survey bottom temperatures](./plots/nbs_ebs_temperature_map.png)
 <font size="2"><i>Figure 3. Contour map of bottom temperatures from the 2021 eastern and northern Bering Sea shelf bottom trawl survey. Solid black lines in the interior of the surface represent stratum boundaries.</i></font>

The coldest waters at the seafloor were restricted to the far northeast corner of the eastern Bering Sea shelf survey area in 2021, where temperatures were greater than -1°C, with an extremely small extent of waters ≤ 1°C (14,925 km2) and ≤ 0°C (4,800 km2). However, cooler bottom temperatures were observed in the northern Bering Sea, including a substantial area of waters ≤ -1°C located west-southwest of St. Lawrence Island, while extremely warm bottom temperatures were observed from Norton Sound to Nunivak Island (Figure 3).

![Mean bottom and sea surface temperatures in the eastern Bering Sea from 1982-2021, based on interpolated survey temperatures](./plots/average_temperature.png)
 <font size="2"><i>Figure 4. Average summer surface (green triangles) and bottom (blue circles) temperatures (°C) of the eastern Bering Sea (EBS) shelf and northern Bering Sea (NBS) shelf based on data collected during standardized summer bottom trawl surveys from 1982–2021. Dashed lines represent the time series mean for the EBS (1982–2021, except 2020) and NBS (2010, 2017, 2019, 2021).</i></font>

Mean surface and bottom temperatures in 2021 were cooler than the prior sampled year (2019), yet remained within one standard deviation above the grand mean of the time series (Figure 4). In 2021, mean bottom temperature was 3.3°C, the fifth highest on record after 2019, 2018, and 2017, and 0.9°C above the grand mean of the time series (2.5°C). The 2021 mean surface temperature was 7.2°C, which was 2.0°C lower than in 2019 yet 0.5°C higher than the grand mean of the time series (6.7°C).

