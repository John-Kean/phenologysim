################################################################################
# Driver data functions
################################################################################


#' Set up temperature data frame for phenology modelling
#'
#' @param data A data frame with columns for date and optionally Tmin, Tmax and/or Tmean
#' @returns A data frame with columns for date, dayfraction and degreesC
#' @export
#'
prepare_temperatures <- function(data) {

  # Check that date column is present
  if (!"date" %in% colnames(data)) stop("No 'date' column present in data!")

  # If only Tmean present then use it for degreesC
  if ("Tmean" %in% colnames(data) && !all(c("Tmin", "Tmax") %in% colnames(data))) {
    return(data |>
      dplyr::rename(degreesC = Tmean) |>
      dplyr::mutate(dayfraction = 1)
    )
  }

  # If only Tmin and Tmax present then estimate degreesC mean from them
  if (!"Tmean" %in% colnames(data) && all(c("Tmin", "Tmax") %in% colnames(data))) {
    data <- data |>
      dplyr::mutate(Tmean = 0.5 * (Tmin + Tmax))
  }

  # Tmin, Tmax and Tmean are present
  if (all(c("Tmin", "Tmax", "Tmean") %in% colnames(data))) {
    return(data |>
      dplyr::mutate(Tmean2 = Tmean) |>
      tidyr::pivot_longer(
        cols = c("Tmin", "Tmean", "Tmax", "Tmean2"),
        names_to = NULL,
        values_to = "degreesC"
      ) |>
     dplyr::mutate(dayfraction = 0.25)
    )
  }

  # Does not appear to be valid data
  stop("No matching format found!")
}


#' Set up daylength data frame for phenology modelling
#'
#' @param data A data frame with columns for date and daylength
#' @returns A data frame with columns for date, daylength and daytrend (1 if daylength is increasing, -1 if it is decreasing)
#' @export
#'
prepare_daylengths <- function(data, latitude = NULL) {

  # Check that date column is present
  if (!"date" %in% colnames(data)) stop("No 'date' column present in data!")
  if (!inherits(data$date, "Date")) stop("'date' must be of class Date")

  # If latitude is specified then get the daylengths
  if (!is.null(latitude)) {
    data <- data |> dplyr::mutate(daylength = geosphere::daylength(latitude, lubridate::yday(date)))
  }

  # Check that daylength column is present
  if (!"daylength" %in% colnames(data)) stop("No 'daylength' column present in data!")

  # Figure out if daylength is increasing or decreasing
  data2 <- data |>
    dplyr::select(date, daylength) |>
    dplyr::distinct() |>
    dplyr::mutate(prev_dl = lag(daylength, default = last(daylength))) |>
    dplyr::mutate(daytrend = dplyr::case_when(
      daylength > prev_dl ~  1,
      daylength < prev_dl ~ -1,
      TRUE                ~  0
    )) |>
    dplyr::select(date, daytrend)
  dplyr::left_join(data, data2, by = "date")
}


#' Set up driver data for phenology modelling
#'
#' This is a shorthand for prepare_temperatures() then prepare_daylengths().
#'
#' @param data A data frame with columns for date, temperature and daylength
#' @returns A data frame with columns for date, dayfraction, degreesC, daylength and daytrend
#' @export
#'
prepare_drivers <- function(data, ...) {
  data |>
    prepare_temperatures() |>
    prepare_daylengths(...) |>
    dplyr::select(date, dayfraction, degreesC, daylength, daytrend)
}


#' Plot drivers
#'
#' @param driver_data A data frame with columns for date, dayfraction, degreesC, daylength and daytrend
#' @returns A ggplot graph
#' @export
#' @examples
#' plot_drivers(example_varying_drivers())
#'
plot_drivers <- function(driver_data) {
  driver_data |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::group_by(date) |>
    dplyr::summarise(
      Tmin = min(degreesC),
      Tmax = max(degreesC),
      Tmean = median(degreesC),
      daylength = mean(daylength)
    ) |>
    tidyr::pivot_longer(
      cols = c(Tmin, Tmax, Tmean),
      names_to = "Datum",
      values_to = "Temperature"
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = date)) +
      ggplot2::geom_line(
        ggplot2::aes(y = Temperature, colour = Datum),
        linewidth = 0.5
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = (daylength - 6) * 30 / 12),
        linewidth = 1.2,
        colour = "darkgrey"
      ) +
      ggplot2::scale_y_continuous(
        name = "Temperature (°C)",
        sec.axis = ggplot2::sec_axis(~ . * 12 / 30 + 6, name = "Daylength (h)")
      ) +
      ggplot2::labs(
        x = "Date",
        colour = ""
      ) +
      ggplot2::theme_classic()
}


