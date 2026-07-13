################################################################################
# Examples of use
################################################################################


#' Make some driver data with constant temperature and daylength
#'
#' @param degC The temperature (°C)
#' @param daylength The daylength (h)
#' @returns Driver data with constant temperature and daylength
#'
#' @examples
#' View(example_constant_drivers())
#' plot_drivers(example_constant_drivers())
#'
example_constant_drivers <- function(degC = 20, daylength = 14) {
  tibble(
    date = seq.Date(as.Date("2024-07-01"), by = "day", length.out = 365),
    Tmean = degC,
    daylength = daylength) |>
    prepare_drivers()
}



#' Make some driver data with varying temperature and daylength
#'
#' @param latitude The latitude of the location (for calculating daylengths)
#' @param degC The mean temperature
#' @param length The number of days returned
#' @returns Some example driver data with varying temperature and daylength
#'
#' @examples
#' View(example_varying_drivers())
#' plot_drivers(example_varying_drivers())
#'
example_varying_drivers <- function(latitude = -37.44, degC = 15, length = 365) {
  cos <- 5 * cos(seq(0, 2 * pi, length.out = length))
  data.frame(
    date = seq.Date(as.Date("2020-07-01"), by = "day", length.out = length),
    Tmin = rnorm(length, mean = degC - 5 - cos, sd = 1),
    Tmax = rnorm(length, mean = degC + 5 - cos, sd = 1),
    Tmean = rnorm(length, mean = degC - cos, sd = 1)
  ) |>
    dplyr::mutate(daylength = geosphere::daylength(lat = latitude, doy = lubridate::yday(date))) |>
    prepare_drivers()
}



#' Example population structure
#'
#' @returns List of life stages
#'
example_lifestages <- function() {
  list(

    eggs = new_chilled_lifestage(
      name = "Eggs",
      devFunction = linear_development,
      devParameters = list(b = 7.4, q = 428),
      varFunction = cum_normal_variability,
      varParameters = list(sd = 0.1),
      chillFunction = linear_chill,
      chillParameters = list(T_chill = 10),
      chillResponse = function(cumChill) { 1 - 0.46 * exp(-0.0005 * cumChill) }
    ),

    larvae = new_lifestage(
      name = "Larvae",
      devFunction = Briere2_development,
      devParameters = list(
        T0 = 12,
        Tu = 33,
        a = 4.7e-5,
        b = 4.2
      ),
      varFunction = Weibull_variability,
      varParameters = list(a = 0.8, c = 3)
    ),

    pupae = new_lifestage(
      name = "Pupae",
      devFunction = linear_development,
      devParameters = list(b = 10, q = 200),
      varFunction = cum_normal_variability,
      varParameters = list(sd = 0.1)
    ),

    adults = new_lifestage(
      name = "Adults",
      devFunction = calendar_development,
      devParameters = list(Q = 14),
      varFunction = cum_normal_variability,
      varParameters = list(sd = 0.1)
    )
  )
}


#' Example timestep
#'
#' @param population List of life stages
#' @param drivers Suitable temperature and daylength data for 1 time step
#' @param survival Stage survival parameters
#' @param fecundity Adult fecundity
#'
#' @returns Updated population after 1 time step
#'
example_timestep <- function(
  population,
  drivers,
  survival = c(
    eggs   = 0.999,
    larvae = 0.99,
    pupae  = 0.99,
    adults = 0.9
  ),
  fecundity = 0.2) {

  p <- population
  d <- drivers

  # Survival and development
  p$eggs   <- p$eggs   |> survive(d, survival["eggs"])   |> develop(d)
  p$larvae <- p$larvae |> survive(d, survival["larvae"]) |> develop(d)
  p$pupae  <- p$pupae  |> survive(d, survival["pupae"])  |> develop(d)
  p$adults <- p$adults |> survive(d, survival["adults"]) |> develop(d)

  # Stages mature
  p$eggs   <- p$eggs   |> add_cohort(rpois(1, total(p$adults) * fecundity))
  p$larvae <- p$larvae |> add_cohort(maturing(p$eggs))
  p$pupae  <- p$pupae  |> add_cohort(maturing(p$larvae))
  p$adults <- p$adults |> add_cohort(maturing(p$pupae))

  p
}



#' Run an example phenology model and plot the results
#'
#' @returns A plot of some example simulation results
#' @param length The number of days to simulate for
#'
#' @example
#' example_phenology_simulation(730)
#'
example_phenology_simulation <- function(length = 365) {
  set.seed(123)
  run_simulation(
    population   = example_lifestages,
    initialise   = c(eggs = 1000),
    timestep     = example_timestep,
    drivers      = example_varying_drivers(length = length),
    survival = c(
      eggs   = 0.999,
      larvae = 0.99,
      pupae  = 0.99,
      adults = 0.9
    ),
    fecundity = 0.2,
    verbose = TRUE
  ) |>
    dplyr::select(date, eggs:adults) |>
    plot_population_trajectory()
}

