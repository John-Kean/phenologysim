################################################################################
# Utility functions
################################################################################


#' Rescale degree day requirement
#'
#' @param oldReq The old day degree requirement (°d)
#' @param oldBase The old base temperature (°C)
#' @param newBase The desired new base temperature (°C) - default is 10 °C
#' @param pivot The pivot temperature (°C) - the default is 25 °C
#' @returns The adjusted day degree requirement (°d)
#'
#' @examples
#' rescale_dd(100, 12)
#'
rescale_dd <- function(oldReq, oldBase, newBase = 10, pivot = 20) {
  oldReq * (pivot - oldBase) / (pivot - newBase)
}



#' Fit a linear day degree model to data
#'
#' @param degC A list of the temperatures
#' @param days A list of the required number of days for development
#' @returns Parameters for the base temperature (°C) and day degree requirement (°d)
#'
#' @examples
#' fit_day_degree_model(c(10, 15, 20, 25, 30), c(50, 25, 12, 6, 4))
#'
fit_day_degree_model <- function(degC, days) {
  # Set up the data
  df <- tibble(x = degC, y = days) |>
    drop_na() |>
    dplyr::mutate(rate = 1 / y)
  # Linear fit to estimate parameters
  c <- coef(lm(rate ~ x, df))
  q_est <- 1 / c[2]
  b_est <- -c[1] / c[2]
  # Non-linear final fit
  fit <- FALSE
  try({ fit <- nls(y ~ q / pmax(0, x - b), data = df, start = list(q = q_est, b = b_est)) })
  if (typeof(fit) == "logical") {
    try({ fit <- nls(y ~ q / pmax(0, x - b), data = df, start = list(q = q_est, b = 10)) })
  }
  c(coef(fit), "corr" = cor(df$y, predict(fit)))
}


