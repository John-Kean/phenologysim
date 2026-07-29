################################################################################
# Class: lifestage
# S3 class
# Use this class to model cohorts of equal-aged life stages, such as insect
#   life cycles, where the development of each stage depends on temperature
#   or calendar time.
################################################################################

#' Constructor for lifestage object
#'
#' @param name Name of the stage e.g. "First instar larvae"
#' @param devFunction The development function to use
#'   e.g. linear_development
#' @param devParameters Named list of parameters for the development function
#'   e.g. c(b = 10, q = 500)
#' @param varFunction The variability function to use
#'   e.g. cum_normal_variability
#' @param varParameters Named list of parameters for the variability function
#'   e.g. c(sd = 0.1)
#' @returns A lifestage object
#' @export
#' @examples
#' larvae = new_lifestage(
#'   name = "Larvae",
#'   devFunction = linear_development,
#'   devParameters = list(b = 10, q = 200),
#'   varFunction = cum_normal_variability,
#'   varParameters = list(sd = 0.1)
#' )
#'
new_lifestage <- function(
    name,
    devFunction,
    devParameters = list(),
    varFunction,
    varParameters = list()
) {
  stopifnot(
    is.function(devFunction),
    is.function(varFunction)
  )

  # Specify development and variability functions for speed
  dev_fun <- function(t, dl, dt) {
    do.call(
      devFunction,
      c(devParameters, list(t = t, dl = dl, dt = dt))
    )
  }
  var_fun <- function(D) {
    do.call(
      varFunction,
      c(list(D = D), varParameters)
    )
  }
  structure(
    list(
      name = name,
      dev_fun = dev_fun,
      var_fun = var_fun,
      devFunction = devFunction,
      varFunction = varFunction,
      devFunctionName = deparse(substitute(devFunction)),
      varFunctionName = deparse(substitute(varFunction)),
      devParameters = devParameters,
      varParameters = varParameters,
      devToday = 0,
      cohorts = tibble::tibble(
        StartNumber    = integer(),
        CurrentNumber  = integer(),
        CumDevelopment = numeric(),
        PropnMatured   = numeric(),
        MaturedToday   = integer(),
        AgeDays        = numeric()
      )
    ),
    class = "lifestage"
  )
}


#' Print method for lifestage objects
#'
#' @param x A lifestage object
#' @export
#' @examples
#'  print(larvae)
#'
print.lifestage <- function(x) {
  cat(x$name, ":\n", sep = "")
  cat("\tDevelopment function: ",
    x$devFunctionName, "(",
    if (length(x$devParameters)) {
      toString(paste(names(x$devParameters), "=", x$devParameters))
    } else "",
    ")\n", sep = ""
  )
  cat("\tVariability function: ",
    x$varFunctionName, "(",
    if (length(x$varParameters)) {
      toString(paste(names(x$varParameters), "=", x$varParameters))
    } else "",
    ")\n", sep = ""
  )
  cat("\tDevelopment today: ", signif(x$devToday, 5), "\n", sep = "")
  n_cohorts <- nrow(x$cohorts)
  if (n_cohorts > 0) {
    cat("\tMatured today: ", sum(x$cohorts$MaturedToday), "\n", sep = "")
    cat("\tTotal number: ", sum(x$cohorts$CurrentNumber), "\n", sep = "")
    cat("\tNumber of cohorts: ", n_cohorts, "\n", sep = "")
    print(x$cohorts)
  } else {
    cat("\tMatured today: 0\n")
    cat("\tTotal number: 0\n")
    cat("\tNumber of cohorts: 0\n")
    cat("\t<no cohorts>\n")
  }
  invisible(x)
}


#' Add a cohort to a lifestage
#'
#' @param x A lifestage object
#' @param number (integer) The number of individuals in the cohort to be added
#' @param cumDevelopment (double) Their prior development (default = 0)
#' @returns The updated lifestage object
#' @export
#' @examples
#'   larvae <- larvae |> add_cohort(100)
#'
add_cohort.lifestage <- function(
    x,
    number,
    cumDevelopment = 0
) {
  if (is.na(number) || number <= 0) return(x)
  p <- x$var_fun(cumDevelopment)
  n <- as.integer(round(number))
  x$cohorts <- dplyr::bind_rows(
    x$cohorts,
    tibble::tibble(
      StartNumber = n,
      CurrentNumber = n,
      CumDevelopment = cumDevelopment,
      PropnMatured = p,
      MaturedToday = 0L,
      AgeDays = 0
    )
  )
  x
}


#' Apply survival to all cohorts in the lifestage
#'
#' @param x A lifestage object
#' @param survival_rate Survival rate /day
#' @param day_fraction Proportion of the day that this applies to (default = 1)
#' @returns The updated lifestage object
#' @export
#' @examples
#'   larvae <- larvae |> survive(0.99, 0.25)
#'
survive.lifestage <- function(
    x,
    survival_rate,
    day_fraction = 1
) {
  s <- max(0, min(1, survival_rate))
  d <- max(0, min(1, day_fraction))
  surv <- s ^ d
  x$cohorts$CurrentNumber <- rbinom(
    nrow(x$cohorts),
    x$cohorts$CurrentNumber,
    surv
  )
  x$cohorts <- dplyr::filter(x$cohorts, CurrentNumber > 0)
  x
}


#' Kill all individuals in the lifestage
#'
#' @param x A lifestage object
#' @returns The updated lifestage object
#' @export
#' @examples
#'   larvae <- larvae |> kill_all()
#'
kill_all.lifestage <- function(x) {
  x$cohorts <- x$cohorts[0, ]
  x
}


#' Develop cohorts for one timestep
#'
#' @param x A lifestage object
#' @param drivers A tibble row with degreesC, daylength, daytrend and
#'   dayfraction
#' @returns The updated lifestage object
#' @export
#' @examples
#'   larvae <- larvae |> develop(d)
#'
develop.lifestage <- function(x, drivers) {

  if (!nrow(x$cohorts)) return(x)
  DT <- x$cohorts
  DT$MaturedToday <- 0L

  # Development
  x$devToday <- x$dev_fun(
    t  = drivers$degreesC,
    dl = drivers$daylength,
    dt = drivers$daytrend
  ) * drivers$dayfraction

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
  DT$MaturedToday  <- rbinom(nrow(DT), DT$CurrentNumber, prob)
  DT$CurrentNumber <- DT$CurrentNumber - DT$MaturedToday
  DT$PropnMatured  <- p
  x$cohorts <- dplyr::filter(DT, CurrentNumber + MaturedToday > 0)
  x
}


#' Get total maturing from lifestage in the current timestep
#'
#' @param x A lifestage object
#' @returns The number maturing from this lifestage in the current timestep
#' @export
#' @examples
#'   maturing(larvae)
#'
maturing.lifestage <- function(x, ...) {
  if (nrow(x$cohorts) == 0) return(0)
  sum(x$cohorts$MaturedToday)
}


#' Get total number in lifestage
#'
#' @param x A lifestage object
#' @returns Total number of individuals in that lifestage, across all cohorts
#' @export
#' @examples
#'   total(larvae)
#'
total.lifestage <- function(x, ...) {
  if (nrow(x$cohorts) == 0) return(0)
  sum(x$cohorts$CurrentNumber)
}


