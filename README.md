
# laggeR

Visualisation of lagged time series and autoregression in R.

## Installation

You can install `laggeR` from GitHub by running the following:

``` r
devtools::install_github("hendersontrent/laggeR")
```

## Motivation

Operations such as autocorrelation function (ACF) and partial
autocorrelation function (PACF) are immensely useful for understanding
the lagged structure of a time series. However, without an immediate
knowledge of how they (or autocorrelation in general) works, it can be
difficult to deeply understand the structure of your time series. This
package automates data visualisations in a basic format (bivariate
scatterplot) and statistical computations so you can more easily see how
well different lagged versions of your time series (or another time
series) predict values of your time series.

## Core functions

### plot\_ar()

Produces a matrix of scatterplots for various time lags and their linear
relationship with future values of your time series.

``` r
library(tidyverse)
library(scales)
library(data.table)
library(laggeR)

# Pull Apple stock time series data as a test

library(tidyquant)
getSymbols("AAPL", warnings = FALSE,
           auto.assign = TRUE)

# Plot with the function

plot_ar(timeseries = as.vector(AAPL$AAPL.Adjusted))
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

You can also specify a vector of time lags if you some a priori
understanding of potentially informative lags.

``` r
plot_ar(timeseries = as.vector(AAPL$AAPL.Adjusted), lags = c(1,30,182,365))
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

You can opt to return a dataframe of statistical model outputs for each
lagged regression instead of a plot.

``` r
outputs <- plot_ar(timeseries = as.vector(AAPL$AAPL.Adjusted), lags = c(1,30,182,365), plot = FALSE)
head(outputs)
```

### plot\_ar\_multiv()

This function extends the univariate time series case presented in
`plot_ar()` to the multivariate space, where lagged values of a time
series `x` are used to predict values in a time series of interest `y`.
This is similar to the concept of [Granger
causality](https://en.wikipedia.org/wiki/Granger_causality).

``` r
library(tidyverse)
library(scales)
library(data.table)
library(laggeR)

# Pull Apple and Microsoft stock time series data as a test

library(tidyquant)
getSymbols("AAPL", warnings = FALSE,
           auto.assign = TRUE)

getSymbols("MSFT", warnings = FALSE,
           auto.assign = TRUE)

# Plot with the function

plot_ar_multiv(timeseriesx = as.vector(MSFT$MSFT.Adjusted),
               timeseriesy = as.vector(AAPL$AAPL.Adjusted))
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Similar to `plot_ar()`, you can also specify a vector of time lags.

``` r
plot_ar_multiv(timeseriesx = as.vector(MSFT$MSFT.Adjusted),
               timeseriesy = as.vector(AAPL$AAPL.Adjusted),
               lags = c(1,30,182,365))
```

![](README_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

You can also opt to return a dataframe of statistical model outputs for
each lagged regression instead of a plot.

``` r
outputs <- plot_ar_multiv(timeseriesx = as.vector(MSFT$MSFT.Adjusted),
               timeseriesy = as.vector(AAPL$AAPL.Adjusted), plot = FALSE)
head(outputs)
```

## Important notes

Functionality does not currently support smoothed fits, such as in a
[generalised additive
model](https://en.wikipedia.org/wiki/Generalized_additive_model), nor do
the statistical tests assess assumptions or perform model diagnostics.
These are slated for future version releases.

## Further work

More functionality is currently under development. Please check back
soon\!
