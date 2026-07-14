################################################################################
# Chilling functions
################################################################################


#' Linear chill function
#'
#' @param t The temperature (°C)
#' @param T_chill The chill threshold temperature (°C)
#' @returns The chill accumulation
#' @export
#'
linear_chill <- function(t, T_chill, ...) {
  pmax(0, T_chill - t)
}


