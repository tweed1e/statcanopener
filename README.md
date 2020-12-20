
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statcanopener

<!-- badges: start -->

<!-- badges: end -->

The goal of statcanopener is to …

## Installation

<!-- You can install the released version of statcanopener from [CRAN](https://CRAN.R-project.org) with: -->

You can install the development version of statcanopener from
[github](https://github/tweed1e/statcanopener) with:

``` r
# install.packages("remotes")
remotes::install_github("tweed1e/statcanopener)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(statcanopener)

## basic example code

getCubeMetadata(35100003)
#> Response [https://www150.statcan.gc.ca/t1/wds/rest/getCubeMetadata]
#>   Date: 2020-12-20 17:37
#>   Status: 406
#>   Content-Type: application/json
#>   Size: 91 B
```
