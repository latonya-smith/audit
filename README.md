
# audit

<!-- badges: start -->
<!-- badges: end -->

The goal of audit is to provide an analysis of a dataframe 

## Installation

You can install the development version of audit like so:

``` r
if(!require(remotes)){
install.packages("remotes")
remotes::install_github("latonya-smith/audit")
}
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(audit)
audit(mtcars)
audit(airquality)
```

