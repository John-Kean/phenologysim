################################################################################
# Class: chilled_lifestage
# S3 subclass of lifestage
# Used when development rate depends on accumulated chilling below a threshold
################################################################################

# Constructor
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
  x$cohorts[, CumChill := numeric()]
  x$chillToday <- 0

  class(x) <- c("chilled_lifestage", class(x))
  x
}


# Add a cohort
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

  x$cohorts <- rbindlist(list(
    x$cohorts,
    data.table(
      StartNumber    = n,
      CurrentNumber  = n,
      CumDevelopment = cumDevelopment,
      PropnMatured   = p,
      MaturedToday   = 0L,
      AgeDays        = 0,
      CumChill       = cumChill
    )
  ), use.names = TRUE)

  x
}

#' Develop cohorts for one timestep
develop.chilled_lifestage <- function(x, drivers) {

  if (!nrow(x$cohorts)) {
    x$devToday   <- 0
    x$chillToday <- 0
    return(x)
  }
  DT <- x$cohorts
  DT[, MaturedToday := 0L]

  # Base development
  d_base <- x$dev_fun(
    t  = drivers$degreesC,
    dl = drivers$daylength,
    dt = drivers$daytrend
  )

  # Chilling accumulation
  x$chillToday <- x$chill_fun(t = drivers$degreesC) * drivers$dayfraction

  # Update cumulative chilling
  DT[, CumChill := CumChill + x$chillToday]

  # Chill response
  rel_dev <- x$chill_response(DT$CumChill)
  rel_dev <- pmin(1, pmax(0, rel_dev))

  # Final development
  x$devToday <- d_base * rel_dev * drivers$dayfraction

  # Update development & age
  DT[, `:=`(
    CumDevelopment = CumDevelopment + x$devToday,
    AgeDays = AgeDays + drivers$dayfraction
  )]

  # Maturation probability
  p <- x$var_fun(DT$CumDevelopment)
  prev <- DT$PropnMatured
  prob <- (p - prev) / (1 - prev)
  prob[prev >= 1] <- 0
  prob <- pmin(pmax(prob, 0), 1)

  # Update cohorts
  DT[, MaturedToday := rbinom(.N, CurrentNumber, prob)]
  DT[, CurrentNumber := CurrentNumber - MaturedToday]
  DT[, PropnMatured := p]
  x$cohorts <- DT[CurrentNumber + MaturedToday > 0]
  x
}


