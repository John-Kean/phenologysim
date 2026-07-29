################################################################################
# Class: chilled_lifestage
# S3 subclass of lifestage
# Used when development rate depends on accumulated chilling below a threshold
################################################################################

#' Constructor
#'
#' @param name Name of the stage e.g. "Eggs"
#' @param devFunction The development function to use
#'   e.g. linear_development
#' @param devParameters Named list of parameters for the development function
#'   e.g. c(b = 10, q = 500)
#' @param varFunction The variability function to use
#'   e.g. cum_normal_variability
#' @param varParameters Named list of parameters for the variability function
#'   e.g. c(sd = 0.1)
#' @param chillFunction The chill accumulation function
#'   e.g. linear_chill
#' @param chillParameters Named list of parameters for the variability function
#'   e.g. c(T_chill = 5)
#' @param chillResponse The function relating the cumulative chill to the
#'   relative development rate (0 to 1).
#' @returns A chilled_lifestage object
#' @export
#' @examples
#' eggs = new_chilled_lifestage(
#'   name = "Eggs",
#'   devFunction = linear_development,
#'   devParameters = c(b = 7.4, q = 428),
#'   varFunction = cum_normal_variability,
#'   varParameters = c(sd = 0.1),
#'   chillFunction = linear_chill,
#'   chillParameters = c(T_chill = 13.9),
#'   chillResponse = function(x) 1 - 0.46 * exp(-0.0005 * x)
#' )
#'
new_chilled_lifestage <- function(
    name,
    devFunction,
    devParameters = list(),
    varFunction,
    varParameters = list(),
    chillFunction,
    chillParameters = list(),
    chillResponse
) {
  stopifnot(
    is.function(devFunction),
    is.function(varFunction),
    is.function(chillFunction),
    is.function(chillResponse)
  )

  x <- new_lifestage(
    name          = name,
    devFunction   = devFunction,
    devParameters = devParameters,
    varFunction   = varFunction,
    varParameters = varParameters
  )

  # Chill behaviour
  x$chill_fun <- function(t) {
    do.call(
      chillFunction,
      c(list(t = t), chillParameters)
    )
  }
  x$chill_response <- chillResponse

  # Extend cohort table
  x$cohorts$CumChill <- numeric(nrow(x$cohorts))
  x$chillToday <- 0

  class(x) <- c("chilled_lifestage", class(x))
  x
}


#' Add a cohort to a chilled_lifestage
#'
#' @param x A chilled_lifestage object
#' @param number (integer) The number of individuals in the cohort to be added
#' @param cumDevelopment (double) Their prior development (default = 0)
#' @param cumChill (double) Their prior chill accumulation (default = 0)
#' @returns The updated chilled_lifestage object
#' @export
#' @examples
#'   eggs <- eggs |> add_cohort(100)
#'
add_cohort.chilled_lifestage <- function(
    x,
    number,
    cumDevelopment = 0,
    cumChill = 0
) {

  if (is.na(number) || number <= 0) return(x)

  # Add cohort via lifestage method
  p <- x$var_fun(cumDevelopment)
  n <- as.integer(round(number))


  x$cohorts <- dplyr::bind_rows(
    x$cohorts,
    tibble::tibble(
      StartNumber    = n,
      CurrentNumber  = n,
      CumDevelopment = cumDevelopment,
      PropnMatured   = p,
      MaturedToday   = 0L,
      AgeDays        = 0,
      CumChill       = cumChill
    )
  )
  x
}


#' Develop cohorts for one timestep
#'
#' @param x A chilled_lifestage object
#' @param drivers Tibble row with degreesC, daylength, daytrend and dayfraction
#' @returns The updated chilled_lifestage object
#' @export
#' @examples
#'   eggs <- eggs |> develop(d)
#'
develop.chilled_lifestage <- function(x, drivers) {

  if (!nrow(x$cohorts)) {
    x$devToday   <- 0
    x$chillToday <- 0
    return(x)
  }
  DT <- x$cohorts
  DT$MaturedToday <- 0L

  # Base development
  d_base <- x$dev_fun(
    t  = drivers$degreesC,
    dl = drivers$daylength,
    dt = drivers$daytrend
  )

  # Chilling accumulation
  x$chillToday <- x$chill_fun(t = drivers$degreesC) * drivers$dayfraction

  # Update cumulative chilling
  DT$CumChill <- DT$CumChill + x$chillToday

  # Chill response
  rel_dev <- x$chill_response(DT$CumChill)
  rel_dev <- pmin(1, pmax(0, rel_dev))

  # Final development
  x$devToday <- d_base * rel_dev * drivers$dayfraction

  # Update development & age
  DT$CumDevelopment <- DT$CumDevelopment + x$devToday
  DT$AgeDays <- DT$AgeDays + drivers$dayfraction

  # Maturation probability
  p <- x$var_fun(DT$CumDevelopment)
  prev <- DT$PropnMatured
  prob <- (p - prev) / (1 - prev)
  prob[prev >= 1] <- 0
  prob <- pmin(pmax(prob, 0), 1)

  # Update cohorts
  DT$MaturedToday <- rbinom(nrow(DT), DT$CurrentNumber, prob)
  DT$CurrentNumber <- DT$CurrentNumber - DT$MaturedToday
  DT$PropnMatured <- p
  x$cohorts <- dplyr::filter(DT, CurrentNumber + MaturedToday > 0)

  x
}


