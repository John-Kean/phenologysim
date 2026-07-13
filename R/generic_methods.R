################################################################################
# Define generic methods for lifestage classes
################################################################################

#' Print method for lifestage objects
#'
#' @param x A lifestage object
#' @export
#' @examples
#'  print(larvae)
print <- function(x, ...) { UseMethod("print") }



#' @export
add_cohort <- function(x, ...) { UseMethod("add_cohort") }



#' @export
survive <- function(x, ...) { UseMethod("survive") }



#' @export
kill_all <- function(x, ...) { UseMethod("kill_all") }



#' @export
develop <- function(x, ...) { UseMethod("develop") }



#' @export
maturing <- function(x, ...) { UseMethod("maturing") }



#' @export
total <- function(x, ...) { UseMethod("total") }


