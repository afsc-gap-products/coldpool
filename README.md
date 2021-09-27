# Eastern Bering Sea Cold Pool Index
_Sean Rohan and Lewis Barnett_

This repository contains a package that is used to calculate the Cold Pool Index for the eastern Bering Sea using bottom temperature data collected during AFSC bottom trawl surveys, where the cold pool index is defined as the area of the NOAA/AFSC/RACE eastern Bering Sea with bottom temperatures less than or equal to 2° Celsius, in square kilometers.

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