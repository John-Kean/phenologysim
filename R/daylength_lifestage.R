################################################################################
# Class: daylength_lifestage
# S3 subclass of lifestage
# Used to model a lifecycle stage that is dependent only on daylength
#   for its completion.
################################################################################

#' Constructor for daylength diapausing life stage
#'
#' @param name The name of the stage e.g. "Diapausing eggs"
#' @param Threshold The required daylength (in hours) for completing the stage (positive for increasing daylength, negative for decreasing)
#' @param ReqTrend The required trend in daylength: 1 for increasing daylength, -1 for decreasing daylength
#' @param Variation The variation around the threshold (in hours)
#' @returns A dl_diapause_stage object
#'
new_daylength_lifestage <- function(
    name,
    Threshold,
    ReqTrend,
    Variation
) {
  structure(
    list(
      name = name,
      dlThreshold = Threshold,
      dlReqTrend = ifelse(ReqTrend > 0, 1L, -1L),
      dlVariation = Variation,
      numberTotal = 0L,
      numberMaturing = 0L,
      PropnMatured = 0,
      devToday = 0
    ),
    class = "daylength_lifestage"
  )
}


#' Print method for daylength_lifestage objects
print.daylength_lifestage <- function(x) {
  cat(x$name, ":\n", sep = "")
  cat("\tDaylength threshold: ", x$dlThreshold, " hours\n", sep = "")
  cat("\tRequired trend: ",
    ifelse(x$dlReqTrend == 1, "increasing", "decreasing"),
    " daylength\n",
    sep = ""
  )
  cat("\tVariation: ", x$dlVariation, " hours\n", sep = "")
  cat("\tMatured today: ", x$numberMaturing, "\n", sep = "")
  cat("\tTotal number: ", x$numberTotal, "\n", sep = "")
  invisible(x)
}


#' Add a cohort to a daylength_lifestage
add_cohort.daylength_lifestage <- function(x, number) {
  if(!is.na(number) && number > 0) {
    x$numberTotal <- x$numberTotal + as.integer(round(number))
  }
  x
}


#' Apply survival to a daylength_lifestage
survive.daylength_lifestage <- function(x, drivers, survival) {
  surv <- exp((survival - 1) * drivers$dayfraction)
  surv <- ifelse(is.na(surv), 0, surv)
  surv <- pmin(pmax(surv, 0), 1)
  size <- ifelse(is.na(x$numberTotal) | x$numberTotal < 0, 0, floor(x$numberTotal))
  x$numberTotal <- rbinom(1, x$numberTotal, surv)
  x
}


#' Kill all individuals in the daylength_lifestage
kill_all.daylength_lifestage <- function(x) {
  x$numberMaturing <- 0
  x$numberTotal <- 0
  x$PropnMatured <- 0
  x
}


#' Develop daylength_lifestage for one timestep
develop.daylength_lifestage <- function(x, drivers) {
  x$numberMaturing <- 0L
  x$devToday <- 0
  if (x$numberTotal == 0 || drivers$daytrend != x$dlReqTrend) return(x)

  p <- 1 / (1 + exp(-(4.6 / x$dlVariation) * x$dlReqTrend * (drivers$daylength - x$dlThreshold)))  # logistic variability
  if (x$PropnMatured >= 0.999) {
    x$numberMaturing <- x$numberTotal
    x$numberTotal <- 0L
    x$PropnMatured <- 0
    return(x)
  }

  prob <- pmax(0, (p - x$PropnMatured) / (1 - x$PropnMatured))
  x$numberMaturing <- rbinom(1, x$numberTotal, prob)
  x$numberTotal <- x$numberTotal - x$numberMaturing
  x$PropnMatured <- p
  if (x$numberTotal == 0) x$PropnMatured <- 0
  x
}


#' Get total maturing from daylength_lifestage in the current timestep
maturing.daylength_lifestage <- function(x, ...) {
  x$numberMaturing
}


#' Get total number in daylength_lifestage
total.daylength_lifestage <- function(x, ...) {
  x$numberTotal
}

