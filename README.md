
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statcanopener

<!-- badges: start -->

[![R build
status](https://github.com/tweed1e/statcanopener/workflows/R-CMD-check/badge.svg)](https://github.com/tweed1e/statcanopener/actions)
<!-- badges: end -->

Statcanopener provides a lightweight wrapper for [Statistics Canada’s
Web Data Service](https://www.statcan.gc.ca/eng/developers/wds). With
it, you can build Statcan’s API queries into your packages, reporting
workflows or Shiny apps.

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

# Get metadata for product ID 35100003
result <- getCubeMetadata(35100003) # returns httr response object
content <- httr::content(result) # extract content of response

str(content[[1]], max.level = 2)
#> List of 2
#>  $ status: chr "SUCCESS"
#>  $ object:List of 21
#>   ..$ responseStatusCode: int 0
#>   ..$ productId         : chr "35100003"
#>   ..$ cansimId          : chr "251-0008"
#>   ..$ cubeTitleEn       : chr "Average counts of young persons in provincial and territorial correctional services"
#>   ..$ cubeTitleFr       : chr "Comptes moyens des adolescents dans les services correctionnels provinciaux et territoriaux"
#>   ..$ cubeStartDate     : chr "1997-01-01"
#>   ..$ cubeEndDate       : chr "2017-01-01"
#>   ..$ frequencyCode     : int 12
#>   ..$ nbSeriesCube      : int 174
#>   ..$ nbDatapointsCube  : int 3468
#>   ..$ releaseTime       : chr "2019-05-09T08:30"
#>   ..$ archiveStatusCode : chr "2"
#>   ..$ archiveStatusEn   : chr "CURRENT - a cube available to the public and that is current"
#>   ..$ archiveStatusFr   : chr "ACTIF - un cube qui est disponible au public et qui est toujours mise a jour"
#>   ..$ subjectCode       :List of 2
#>   ..$ surveyCode        :List of 1
#>   ..$ dimension         :List of 2
#>   ..$ footnote          :List of 26
#>   ..$ correctionFootnote: list()
#>   ..$ geoAttribute      : list()
#>   ..$ correction        : list()
```

## Related packages

This package was inspired by the
[cansim](https://github.com/mountainMath/cansim) package, which includes
this functionality and much more. This is intended to be a bare bones,
non-opinionated wrapper for the web data service, rather than a
full-fledged end-user API access package. (E.g., this won’t have a
function to automatically scale from millions of $ to $.)
