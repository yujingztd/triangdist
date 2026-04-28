
<!-- README.md is generated from README.Rmd. Please edit that file -->

# triangdist

The triangdist package provides comprehensive statistical functions for
calculating and simulating the triangular distribution in R. It includes
tools for density (dtriang), cumulative distribution (ptriang),
quantiles (qtriang), and random number generation (rtriang).

# Installation

You can install the development version of triangdist directly from
GitHub using the devtools package:

``` r
# install.packages("devtools")
devtools::install_github("yujingztd/triangdist")
```

# Examples of use

``` r
library(triangdist)

# 1. Calculate the probability density function at the mode (x = 1)
dtriang(x = 1, min = 0, max = 3, mode = 1)
#> [1] 0.6666667

# 2. Generate 5 random values following this distribution
rtriang(n = 5, min = 0, max = 3, mode = 1)
#> [1] 1.7558408 0.5680494 2.3656871 0.7381726 1.3817308
```
