#------------------------------------
# This script sets out to test the
# functions produced in the laggeR
# package
#------------------------------------

#----------------------------------------
# Author: Trent Henderson 12 January 2021
#----------------------------------------

library(tidyverse)
library(scales)
library(data.table)
library(laggeR)

# For extracting some financial time series as a test

library(tidyquant)

#-------------------- plot_ar() ---------------------------

getSymbols("AAPL", warnings = FALSE,
           auto.assign = TRUE)

# No specified lags

plot_ar(timeseries = as.vector(AAPL$AAPL.Adjusted))

# Specified lags

plot_ar(timeseries = as.vector(AAPL$AAPL.Adjusted), lags = c(1,30,182,365))

#-------------------- plot_ar_multiv() --------------------

getSymbols("AAPL", warnings = FALSE,
           auto.assign = TRUE)

getSymbols("MSFT", warnings = FALSE,
           auto.assign = TRUE)

# No specified lags

plot_ar_multiv(timeseriesx = as.vector(MSFT$MSFT.Adjusted),
               timeseriesy = as.vector(AAPL$AAPL.Adjusted))

# Specified lags

plot_ar_multiv(timeseriesx = as.vector(MSFT$MSFT.Adjusted),
               timeseriesy = as.vector(AAPL$AAPL.Adjusted),
               lags = c(1,30,182,365))
