#'
#' Function to automate the production of a set of
#' time series graphs that visualises the multivariate autoregressive
#' structure of two input time series or produces linear regression
#' outputs for each specified time lag
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @import tidyr
#' @importFrom data.table rbindlist
#' @importFrom magrittr %>%
#' @importFrom broom glance
#' @param timeseriesx an input time series as a vector of numerical values
#' @param timeseriesy an input time series as a vector of numerical values
#' @param lags a vector of numeric values that represent the time lags the user wants to visualise
#' @param plot a Boolean of whether to plot the result or return a dataframe
#' @return an object of class `ggplot` that represents a matrix of scatterplots if plot = TRUE, otherwise returns a dataframe of linear model results
#' @author Trent Henderson
#'

plot_ar_multiv <- function(timeseriesx, timeseriesy, lags = NULL, plot = TRUE){

  options(scipen = 999)

  if(!is.vector(timeseriesx) | !is.vector(timeseriesy)){
    stop("Both input time series should be a vector of numerical values.")
  }

  if(all(!is.numeric(timeseriesx)) | all(!is.numeric(timeseriesy))){
    stop("Both input time series should be a vector of numerical values.")
  }

  plots <- c(TRUE, FALSE)
  '%ni%' <- Negate('%in%')

  if(plot %ni% plots){
    stop("Plot argument should be TRUE or FALSE.")
  }

  # Work through potential user-inputted time lags (i.e. seasonality) they
  # are aware of beforehand

  if(is.null(lags)){

    # Dynamically create appropriate lag max based on time series length

    lag <- as.integer((length(timeseriesx)/2))
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
  mods <- list()

  #----------
  # NO INPUTS
  #----------

  if(user == "No"){

    for(i in 1:lag){
      tmp <- data.frame(x = timeseriesx,
                        y = timeseriesy) %>%
        dplyr::mutate(x = lag(x, n = i)) %>%
        dplyr::mutate(lag = i)

      # Run linear regression model to extract p values

      m <- lm(y ~ x, data = tmp)

      # Add statistical outputs

      mod_outs <- broom::glance(m) %>%
        dplyr::mutate(lag = i)

      # Store in the list to loop iteration

      store[[i]] <- tmp
      mods[[i]] <- mod_outs
    }

    outs <- data.table::rbindlist(store, use.names = TRUE)
    mod_outs <- data.table::rbindlist(mods, use.names = TRUE)
  }

  #------------
  # USER INPUTS
  #------------

  if(user == "Yes"){

    for(i in lag){
      tmp <- data.frame(x = timeseriesx,
                        y = timeseriesy) %>%
        dplyr::mutate(x = lag(x, n = i)) %>%
        dplyr::mutate(lag = i)

      # Run linear regression model to extract p values

      m <- lm(y ~ x, data = tmp)

      # Add statistical outputs

      mod_outs <- broom::glance(m) %>%
        dplyr::mutate(lag = i)

      # Store in the list to loop iteration

      store[[i]] <- tmp
      mods[[i]] <- mod_outs
    }

    outs <- data.table::rbindlist(store, use.names = TRUE)
    mod_outs <- data.table::rbindlist(mods, use.names = TRUE)
  }

  if(plot){

    #---- Produce matrix of plots -------------------

    p <- outs %>%
      tidyr::drop_na() %>%
      ggplot2::ggplot(aes(x = x, y = y))

    if(length(timeseriesx) <= 40){
      p <- p +
        ggplot2::geom_point(size = 2.5, colour = "steelblue2")
    } else{
      p <- p +
        ggplot2::geom_point(colour = "steelblue2")
    }

    p <- p +
      ggplot2::geom_smooth(formula = y ~ x, method = "lm", colour = "#331a38", se = TRUE) +
      ggplot2::labs(title = "Visualisation of multivariate time series autoregression at different lags",
                    x = expression(x[t-p]),
                    y = expression(y[t])) +
      ggplot2::theme_bw() +
      ggplot2::facet_wrap(~lag) +
      ggplot2::theme(legend.position = "bottom",
                     panel.grid.minor = ggplot2::element_blank())

    # Make tick mark labels prettier if numbers are large

    if(max(timeseriesx) > 1000){
      p <- p +
        ggplot2::scale_x_continuous(labels = scales::comma)
    }

    if(max(timeseriesy) > 1000){
      p <- p +
        ggplot2::scale_y_continuous(labels = scales::comma)
    }
    return(p)

  } else{
    return(mod_outs)
  }
}
