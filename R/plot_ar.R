#'
#' Function to automate the production of a set of
#' time series graphs that visualise the autoregressive
#' structure of an input time series
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @import tidyr
#' @importFrom data.table rbindlist
#' @importFrom magrittr %>%
#' @param timeseries the input time series as a vector of numerical values
#' @param lags a vector of numeric values that represent the time lags the user wants to visualise
#' @return an object of class `ggplot` that represents a matrix of scatterplots
#' @author Trent Henderson
#'

plot_ar <- function(timeseries, lags = NULL){

  options(scipen = 999)

  if(!is.vector(timeseries)){
    stop("Input timeseries should be a vector of numerical values.")
  }

  if(all(!is.numeric(timeseries))){
    stop("Input timeseries should be a vector of numerical values.")
  }

  # Work through potential user-inputted time lags (i.e. seasonality) they
  # are aware of beforehand

  if(is.null(lags)){

    # Dynamically create appropriate lag max based on time series length

    lag <- as.integer((length(timeseries)/2))
    user <- "No"

    if(lag > 20){
      lag <- 20
      message("Input time series is long and would produce a large number of plots. Limiting output to first 20 lag orders.")
    }
  } else{

    if(!is.vector(lags) | !is.numeric(lags)){
      stop("Lags should be a vector of numerical values.")
    }

    if(all(!is.numeric(lags))){
      stop("Lags should be a vector of numerical values.")
    }

    lag <- lags
    user <- "Yes"
  }

  #---- Calculate all lagged combinations ---------

  store <- list()

  #----------
  # NO INPUTS
  #----------

  if(user == "No"){

    for(i in 1:lag){
      tmp <- data.frame(y = timeseries) %>%
        dplyr::mutate(x = lag(y, n = i)) %>%
        dplyr::mutate(lag_val = paste0("Lag = ", i))

      # Run linear regression model to extract p values

      m <- lm(y ~ x, data = tmp)

      pval <- round(summary(m)$coefficients[,"Pr(>|t|)"][[2]], digits = 3)
      rsq <- round(summary(m)$r.squared, digits = 2)

      if(pval < .001){
        pval <- paste0("p < .001", ", Rsq: ", rsq)
      } else{
        pval <- paste0("p = ", as.character(pval), ", Rsq: ", rsq)
      }

      # Add p values to core dataframe

      tmp1 <- tmp %>%
        dplyr::mutate(pval = pval) %>%
        dplyr::mutate(combo = paste0(lag_val, "\n", pval))

      # Store in the list to loop iteration

      store[[i]] <- tmp1
    }

    outs <- data.table::rbindlist(store, use.names = TRUE)
  }

  #------------
  # USER INPUTS
  #------------

  if(user == "Yes"){

    for(i in lag){
      tmp <- data.frame(y = timeseries) %>%
        dplyr::mutate(x = lag(y, n = i)) %>%
        dplyr::mutate(lag_val = paste0("Lag = ", i))

      # Run linear regression model to extract p values

      m <- lm(y ~ x, data = tmp)

      pval <- round(summary(m)$coefficients[,"Pr(>|t|)"][[2]], digits = 3)
      rsq <- round(summary(m)$r.squared, digits = 2)

      if(pval < .001){
        pval <- paste0("p < .001", ", Rsq: ", rsq)
      } else{
        pval <- paste0("p = ", as.character(pval), ", Rsq: ", rsq)
      }

      # Add p values to core dataframe

      tmp1 <- tmp %>%
        dplyr::mutate(pval = pval) %>%
        dplyr::mutate(combo = paste0(lag_val, "\n", pval))

      # Store in the list to loop iteration

      store[[i]] <- tmp1
    }

    outs <- data.table::rbindlist(store, use.names = TRUE)
  }

  #---- Produce matrix of plots -------------------

  p <- outs %>%
    tidyr::drop_na() %>%
    ggplot2::ggplot(aes(x = x, y = y))

  if(length(timeseries) <= 40){
    p <- p +
      ggplot2::geom_point(size = 2.5, colour = "steelblue2")
  } else{
    p <- p +
      ggplot2::geom_point(colour = "steelblue2")
  }

  p <- p +
    ggplot2::geom_smooth(formula = y ~ x, method = "lm", colour = "#331a38", se = TRUE) +
    ggplot2::labs(title = "Visualisation of input time series autoregression at different lags",
         x = expression(y[t-p]),
         y = expression(y[t])) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(~combo) +
    ggplot2::theme(legend.position = "bottom",
                   panel.grid.minor = ggplot2::element_blank())

  # Make tick mark labels prettier if numbers are large

  if(max(timeseries) > 1000){
    p <- p +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::scale_y_continuous(labels = scales::comma)
  }

  return(p)
}
