
<!-- README.md is generated from README.Rmd. Please edit that file -->

# noaaquake

<!-- badges: start -->

<!-- badges: end -->

The goal of noaaquake is to clean and visualize raw NOAA earthquake data

## Installation

You can install the released version of noaaquake from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("noaaquake")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("adeel1997/noaaquake")
```

## Example

This is a basic example which shows you how one can visualize the data
from the package

    library(noaaquake)
    data_clean <- NOAA_data %>%
     eq_clean_data() %>%
     dplyr::filter(Country == "India"|Country == "Pakistan" & lubridate::year(date) >= 2000)
     eq_map(data = data_clean, annot_col = "Mag")