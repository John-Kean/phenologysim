################################################################################
# Simulation functions
################################################################################


#' Run a simulation (generic handler)
#'
#' @param population (function)  function that creates a list of population life stage objects
#' @param initialise (list)      initial values for non-zero life stages
#' @param timestep   (function)  function used for the timestep
#' @param drivers    (dataframe) with columns for date, dayfraction, degreesC, daylength and daytrend
#' @param verbose    (boolean)   whether to print progress
#' @param ...                    additional parameters required by the timestep function
#' @returns (dataframe) drivers with JulianDate, days_passed and life stage totals added as new columns
#' @export
#' @examples
#' results <- run_simulation(
#'   population = example_lifestages,
#'   initialise = c(eggs = 1000),
#'   timestep   = example_timestep,
#'   drivers    = driver_data
#' )
#'
run_simulation <- function(population, initialise, timestep, drivers, verbose = FALSE, ...) {

  # Initialise population
  pop <- population()
  stage_names <- names(pop)
  for (stage in names(initialise)) {
    pop[[stage]] <- add_cohort(pop[[stage]], initialise[[stage]])
  }

  # Set up results
  results <- drivers |>
    dplyr::mutate(
      JulianDate = as.integer(format(date, "%j")),
      days_passed = cumsum(dayfraction),
    )
  for (stage in stage_names) {
    results[[stage]] <- NA_real_
  }

  # Unpack drivers for maximum speed
  dates       <- drivers$date
  dayfraction <- drivers$dayfraction
  degreesC    <- drivers$degreesC
  daylength   <- drivers$daylength
  daytrend    <- drivers$daytrend

  # Timestep loop
  for (i in seq_len(nrow(drivers))) {
    drivers_now <- list(
      date        = dates[i],
      dayfraction = dayfraction[i],
      degreesC    = degreesC[i],
      daylength   = daylength[i],
      daytrend    = daytrend[i]
    )

    # Show progress
    if (verbose && i %% 28 == 0) cat("\r Simulating date =", format(drivers_now$date))

    # Advance one timestep
    pop <- timestep(population = pop, drivers = drivers_now, ...)

    # Record totals
    for (stage in stage_names) {
      results[[stage]][i] <- total(pop[[stage]])
    }
  }

  # Ensure newline after progress
  if (verbose) cat("\n")

  # Return results
  results
}



#' Return total population size across all lifestages
#'
#' @param pop A list of lifestage objects
#' @returns Numeric sum of all total(lifestage)
#' @export
#'
total_population <- function(pop) {
  sum(sapply(pop, total), na.rm = TRUE)
}



#' Remove all individuals across all lifestages
#'
#' @param pop A list of lifestage objects
#' @returns Updated list of lifestage objects
#' @export
#'
eradicate_population <- function(pop) {
  lapply(pop, \(x) if (is.null(x)) x else kill_all(x))
}



#' Plot a population trajectory
#'
#' @param population_data Data with the first column for the x-axis and remaining columns being life stages to be plotted
#' @param type The plot type: either "area" (default) or "line"
#' @param x_label The label for the x-axis
#' @returns A plot of the simulation results
#' @export
#'
plot_population_trajectory <- function(population_data, type = "area", x_label = "") {
  x_col <- colnames(population_data)[1]
  stages <- colnames(population_data)[-1]
  if (x_label == "") x_label <- stringr::str_to_sentence(gsub("_", " ", x_col))
  population_data |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(stages),
      names_to = "Lifestage",
      values_to = "Population"
    ) |>
    dplyr::mutate(Lifestage = factor(Lifestage, levels = stages)) |>  # sort into the right order
    ggplot2::ggplot(
      ggplot2::aes(x = .data[[x_col]], y = Population)
    ) +
    {
      if(type == "area")
        ggplot2::geom_area(
          ggplot2::aes(fill = Lifestage)
        )
    } +
    {
      if(type == "line")
        ggplot2::geom_line(
          ggplot2::aes(colour = Lifestage),
          linewidth = 1
        )
    } +
    ggplot2::xlab(x_label) +
    ggplot2::theme_classic()
}


