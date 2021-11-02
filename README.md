# Eastern Bering Sea Cold Pool Index
_Sean Rohan and Lewis Barnett_

This repository contains a package that is used to calculate the Cold Pool Index for the eastern Bering Sea using bottom temperature data collected during AFSC bottom trawl surveys, where the cold pool index is defined as the area of the NOAA/AFSC/RACE eastern Bering Sea bottom trawl survey footprint with bottom temperatures less than or equal to 2° Celsius, in square kilometers.

This repository also includes an analysis to compare interpolation methods for calculating the cold pool index.

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
# Cold pool area and mean bottom temperature

![Cold pool area from 1982-2021, based on interpolated survey bottom temperatures](./plots/coldpool_with_area.png)
_Figure 1. Cold pool extent in the eastern Bering Sea from 2002–2021, showing areas with bottom temperatures ≤ 2°C, ≤ 1°C, ≤ 0°C, and ≤ -1°C (upper panels), and proportion of the southeastern Bering Sea survey area with bottom temperatures ≤ 2°C, ≤ 1°C, ≤ 0°C, and ≤ -1°C (lower panel)._

![Mean bottom temperature in the eastern Bering Sea during 2021, based on interpolated survey bottom temperatures](./plots/nbs_ebs_temperature_map.png)
_Figure 2. Contour map of bottom temperatures from the 2021 eastern and northern Bering Sea shelf bottom trawl survey._

![Mean bottom and sea surface temperatures in the eastern Bering Sea from 1982-2021, based on interpolated survey temperatures](./plots/average_temperature.png)
_Figure 3. Average summer surface (green triangles) and bottom (blue circles) temperatures (°C) of the eastern Bering Sea (EBS) shelf and northern Bering Sea (NBS) shelf based on data collected during standardized summer bottom trawl surveys from 1982–2021. Dashed lines represent the time series mean for the EBS (1982–2021, except 2020) and NBS (2010, 2017, 2019, 2021)._