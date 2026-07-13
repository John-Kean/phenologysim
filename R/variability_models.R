################################################################################
# Variability functions
################################################################################


#' Step variability
#'
#' @param D The developmental accumulation
#' @param m The median of the curve (default = 1)
#' @returns Probability of actual development
#' @export
#' @examples
#' plot(seq(0, 2, 0.01), step_variability(seq(0, 2, 0.01)), type = "l", xlab = "Developmental accumulation", ylab = "Probability of development")
#' plot(seq(0, 2, 0.01), step_variability(seq(0, 2, 0.01), m = 0.5), type = "l", xlab = "Developmental accumulation", ylab = "Probability of development")
#'
step_variability <- function(D, m = 1) {
  ifelse (D >= m, 1, 0)
}


#' Logistic variability
#'
#' @param D The developmental accumulation
#' @param w The width of the curve, equivalent to 3*sd. At x = m - w, y = 0.01 and at x = m + w, y = 0.99
#' @param m The mean of the curve (default = 1)
#' @returns Probability of actual development
#' @export
#' @examples
#' plot(seq(0, 2, 0.01), logistic_variability(seq(0, 2, 0.01), w = 0.5), type = "l", xlab = "Developmental accumulation", ylab = "Probability of development")
#' plot(seq(6, 18, 0.01), logistic_variability(seq(6, 18, 0.01), w = 1, m = 14), type = "l", xlab = "Daylength (h)", ylab = "Proportion breaking diapause")
#'
logistic_variability <- function(D, w, m = 1) {
  if (w <= 0) return(step_variability(D, m))
  1 / (1 + exp(-(4.6 / w) * (D - m)))
}


#' Regniere 1 variability
#'
#' See Régnière (1984, Can. Ento. 116:1367-1376).
#'
#' @param D The developmental accumulation
#' @param a A parameter determining the steepness of the curve
#' @param b A parameter determining the skew
#' @param m The median of the curve (default = 1)
#' @returns Probability of actual development
#' @export
#' @examples
#' plot(seq(0, 2, 0.01), regniere1_variability(seq(0, 2, 0.01), a = 20, b = 2), type = "l", xlab = "Developmental accumulation", ylab = "Probability of development")
#'
regniere1_variability <- function(D, a, b, m = 1) {
  (1 + exp(-a * (D - m)) * ((0.5) ^ (-b) - 1)) ^ (-1 / b)
}


#' Regniere 2 variability
#'
#' See Régnière (1984, Can. Ento. 116:1367-1376).
#'
#' @param D The developmental accumulation
#' @param a A parameter determining the steepness of the curve
#' @param b A parameter determining the skew
#' @param m A parameter determining the position of the curve (default = 1.073)
#' @returns Probability of actual development
#' @export
#' @examples
#' plot(seq(0, 2, 0.01), regniere2_variability(seq(0, 2, 0.01), a = 15, b = 2), type = "l", xlab = "Developmental accumulation", ylab = "Probability of development")
#'
regniere2_variability <- function(D, a, b, m = 1.073) {
  (1 + exp(-a * (D - m))) ^ (-1 / b)
}


#' Weibull variability
#'
#' This is flexible implementation. Parameters may be specified as (b,c), (a,b,c), (a,b,m) or (a,c,m)
#'
#' @param D The developmental accumulation
#' @param a The minimum developmental accumulation required (default = 0)
#' @param b The scale parameter (width of the curve) (default = NA)
#' @param c The shape of the curve (default = NA)
#' @param m The median of the curve (default = 1)
#' @returns Probability of actual development
#' @export
#' @examples
#' plot(seq(0, 2, 0.01), weibull_variability(seq(0, 2, 0.01), a = 0.5, c = 3.39), type = "l", xlab = "Developmental accumulation", ylab = "Probability of development")
#' plot(seq(0, 10, 0.01), weibull_variability(seq(0, 10, 0.01), a = 4, b = 2.2, c = 4), type = "l", xlab = "Developmental accumulation", ylab = "Probability of development")
#' plot(seq(0, 10, 0.01), weibull_variability(seq(0, 10, 0.01), a = 4, b = 2.2, m = 6), type = "l", xlab = "Developmental accumulation", ylab = "Probability of development")
#' plot(seq(0, 10, 0.01), weibull_variability(seq(0, 10, 0.01), a = 4, c = 4, m = 6), type = "l", xlab = "Developmental accumulation", ylab = "Probability of development")
#'
weibull_variability <- function(D, a = 0, b = NA, c = NA, m = 1) {
  if (is.na(b) & is.na(c)) stop("You must specify at least b or c for Weibull_variability")
  if (is.na(a) | is.na(b) | is.na(c)) {
    if (is.na(a)) a <- m - (b * log(2) ^ (1/c))
    if (a >= m) return(step_variability(D, m))
    if (is.na(b)) b <- (m - a) / (log(2) ^ (1/c))
    if (is.na(c)) c <- log(log(2)) / (log(m - a) - log(b))
  }
  if (is.na(a) | is.na(b) | is.na(c)) stop("You must specify three parameters for Weibull_variability")
  ifelse(D < a, 0, 1 - exp(-((D - a) / b) ^ c))
}


#' Cumulative normal variability
#'
#' @param D The developmental accumulation (1 = average required)
#' @param sd The standard deviation
#' @param m The median of the curve (default = 1)
#' @returns Probability of actual development
#' @export
#' @examples
#' plot(seq(0, 2, 0.01), cum_normal_variability(seq(0, 2, 0.01), sd = 0.1), type = "l", xlab = "Developmental accumulation", ylab = "Probability of development")
#'
cum_normal_variability <- function(D, sd, m = 1) {
  if (sd == 0) return(step_variability(D, m))
  pnorm(D, mean = m, sd = sd)
#  weibull_variability(D, a = m * (1 - 3 * sd), c = 3.39, m = m)
}


