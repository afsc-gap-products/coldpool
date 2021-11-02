# Eastern Bering Sea Cold Pool Index
_Sean Rohan and Lewis Barnett_

This repository contains a package that is used to calculate the *cold pool index*, mean sea surface temperature, and mean bottom temperature using temperature data collected during bottom trawl surveys of the eastern Bering Sea conducted by NOAA/AFSC/RACE's Groundfish Assessment Program. The cold pool index is defined as the area of the NOAA/AFSC/RACE eastern Bering Sea bottom trawl survey footprint with bottom temperatures less than or equal to 2° Celsius, in square kilometers. This package includes temperature products (mean temperatures cold pool area, interpolated temperature raster) that are updated on an annual basis following the eastern Bering Sea shelf bottom trawl survey. 

The most recent version of this package was developed and tested using R 4.1.1.

# Installation

1. Install the [akgfmaps package](https://github.com/sean-rohan-NOAA/akgfmaps) from GitHub prior to installing coldpool, as follows:
```{r}
devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE)
```

If you encounter problems installing the akgfmaps package, please refer to the akgfmaps GitHub repository.

2. Install the coldpool package using the following code:
```{r}
devtools::install_github("sean-rohan-noaa/coldpool")
```

# Usage

## Accessing datasets
Users can access temperature products using directly from datasets that are built into the package. For example, after installing the package, users can access a data frame containing cold pool area (area with temperature less than or equal to 2°C), area of other isotherms (less than or equal to 1,0,-1 °C), mean bottom temperature for the EBS, and mean surface temperature, using:
```{r}
coldpool:::cold_pool_index
```

Documentation for the dataset can be accessed using:
```{r}
?coldpool:::cold_pool_index
```

## Datasets in the package

Datasets included in the package are:
<li><b>cold_pool_index</b>: Data frame containing areas with temperatures less than or equal to 2, 1,0, and -1 °C, mean bottom temperatures for the EBS, and mean surface temperature during the EBS survey for 1982-2021 (excluding 2020 due to cancelled survey).</li>
<li><b>nbs_mean_bottom_temperature</b>: Data frame of mean bottom temperature in the NBS during years with a full EBS+NBS standardized survey (2010, 2017, 2019, 2021).</li>
<li><b>ebs_bottom_temperature</b>: Interpolated rasters of bottom temperature for the EBS survey area from 1982-2021 (excluding 2020 due to cancelled survey).</li>
<li><b>ebs_surface_temperature</b>: Interpolated rasters of sea surface temperature for the EBS survey area from 1982-2021 (excluding 2020 due to cancelled survey).</li>
<li><b>nbs_ebs_bottom_temperature</b>: Interpolated rasters of bottom temperature for the full EBS and NBS survey area for years with a full EBS+NBS standardized survey (2010, 2017, 2019, 2021).</li>
<li><b>nbs_ebs_surface_temperature</b>: Interpolated rasters of sea surface temperature for the full EBS and NBS survey area for years with a full EBS+NBS standardized survey (2010, 2017, 2019, 2021).</li>
<li><b>cpa_pre2021</b>: Data frame of cold pool area and mean temperatures calculated using the interpolation method used prior to 2021.</li>


# Cold pool area and temperature trends
*Updated: November 2021*

Cold pool area and temperature trends are reported in the annual [Ecosystem Status Reports](https://www.fisheries.noaa.gov/alaska/ecosystems/ecosystem-status-reports-gulf-alaska-bering-sea-and-aleutian-islands) for the eastern Bering Sea and ecosystem and socioeconomic profiles for EBS stocks. Temperature products are also used as covariates in some [stock assessment](https://www.fisheries.noaa.gov/alaska/population-assessments/north-pacific-groundfish-stock-assessments-and-fishery-evaluation) models.

![Cold pool area from 1982-2021, based on interpolated survey bottom temperatures](./plots/coldpool_with_area.png)
 <font size="3"> _Figure 1. Cold pool extent in the eastern Bering Sea from 2002–2021, showing areas with bottom temperatures ≤ 2°C, ≤ 1°C, ≤ 0°C, and ≤ -1°C (upper panels), and proportion of the southeastern Bering Sea survey area with bottom temperatures ≤ 2°C, ≤ 1°C, ≤ 0°C, and ≤ -1°C (lower panel)._</font>

The cold pool extent has increased since 2018, yet the 2021 extent (58,975 km2) was the fourth lowest on record and remains one to two standard deviations below the grand mean of the time series (Figure 1). Estimates from 2018 and 2019 were the lowest on record, followed by 2003, which was only slightly lower than the 2021 estimate. In general, the extent of isotherms at all thresholds ≤ 1°C were similar, if slightly greater than prior record lows (Figure 1). As is typical when the extent is small, the cold pool was restricted to the northern edge of the eastern Bering Sea shelf (Figure 1). 

![Mean bottom temperature in the eastern Bering Sea during 2021, based on interpolated survey bottom temperatures](./plots/nbs_ebs_temperature_map.png)
 <font size="3"> _Figure 2. Contour map of bottom temperatures from the 2021 eastern and northern Bering Sea shelf bottom trawl survey._</font>

The coldest waters at the seafloor were restricted to the far northeast corner of the eastern Bering Sea shelf survey area, where temperatures were greater than -1°C, with an extremely small extent of waters ≤ 1°C (14,925 km2) and ≤ 0°C (4,800 km2). However, cooler bottom temperatures were observed in the northern Bering Sea, including a substantial area of waters ≤ -1°C located west-southwest of St. Lawrence Island, while extremely warm bottom temperatures were observed from Norton Sound to Nunivak Island (Figure 2).

![Mean bottom and sea surface temperatures in the eastern Bering Sea from 1982-2021, based on interpolated survey temperatures](./plots/average_temperature.png)
 <font size="3"> _Figure 3. Average summer surface (green triangles) and bottom (blue circles) temperatures (°C) of the eastern Bering Sea (EBS) shelf and northern Bering Sea (NBS) shelf based on data collected during standardized summer bottom trawl surveys from 1982–2021. Dashed lines represent the time series mean for the EBS (1982–2021, except 2020) and NBS (2010, 2017, 2019, 2021)._</font>

Mean surface and bottom temperatures were cooler than the prior sampled year (2019), yet remained within one standard deviation above the grand mean of the time series (Figure 3). In 2021, mean bottom temperature was 3.3°C, the fifth highest on record after 2019, 2018, and 2017, and 0.9°C above the grand mean of the time series (2.5°C). The 2021 mean surface temperature was 7.2°C, which was 2.0°C lower than in 2019 yet 0.5°C higher than the grand mean of the time series (6.7°C).

