################################################################################
#' Development functions
################################################################################
#'
#' Custom functions can be defined, within the following restrictions.
#'
#' 1. Available drivers are:
#'    @param t The temperature (°C)
#'    @param dl The daylength (hours of light)
#'    @param dt The daylength trend (1 if increasing, -1 if decreasing)
#'
#' 2. Must allow for additional unused parameters:
#'    @param ... Unused variables
#'
#' 3. Must return the proportion of the total developmental requirement that is
#'    completed today (/d):
#'    @returns Proportion of the total developmental requirement completed
#'
################################################################################


#' Analytis 1 development model
#'
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520).
#'
#' @param t The temperature (°C)
#' @param T0 The lower temperature threshold (°C)
#' @param Tu The upper temperature threshold (°C)
#' @param a A parameter controlling the height of the curve
#' @param b A parameter controlling skew of the curve
#' @param c A parameter controlling concavity of the curve
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   analytis1_development(
#'     t  = 0:35,
#'     T0 = 5,
#'     Tu = 33,
#'     a  = 1,
#'     b  = 3,
#'     c  = 0.5
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
analytis1_development <- function(t, T0, Tu, a, b, c, ...) {
  delta <- (t - T0) / (Tu - T0)
  pmax(0, ifelse((t > Tu) | (t < T0), 0, a * delta ^ b * (1 - delta) ^ c))
}


#' Analytis 2 development model
#'
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520).
#'
#' @param t The temperature (°C)
#' @param T0 The lower temperature threshold (°C)
#' @param Tu The upper temperature threshold (°C)
#' @param a A parameter controlling the height of the curve
#' @param b A parameter controlling skew of the curve
#' @param c A parameter controlling concavity of the curve
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   analytis2_development(
#'     t  = 0:35,
#'     T0 = 5,
#'     Tu = 33,
#'     a  = 1,
#'     b  = 5,
#'     c  = 1
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
analytis2_development <- function(t, T0, Tu, a, b, c, ...) {
  delta <- (t - T0) / (Tu - T0)
  pmax(0, ifelse((t > Tu) | (t < T0), 0, (a * delta ^ b * (1 - delta)) ^ c))
}


#' Analytis 3 development model
#'
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520).
#'
#' @param t The temperature (°C)
#' @param T0 The lower temperature threshold (°C)
#' @param Tu The upper temperature threshold (°C)
#' @param a A parameter controlling the height of the curve
#' @param b A parameter controlling skew of the curve
#' @param c A parameter controlling concavity of the curve
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   analytis3_development(
#'     t  = 0:35,
#'     T0 = 5,
#'     Tu = 33,
#'     a  = 0.000005,
#'     b  = 3,
#'     c  = 1
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
analytis3_development <- function(t, T0, Tu, a, b, c, ...) {
  pmax(0, ifelse((t > Tu) | (t < T0), 0, a * (t - T0) ^ b * (Tu - t) ^ c))
}


#' Analytis-Allahyari development model
#'
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520).
#'
#' @param t The temperature (°C)
#' @param T0 The lower temperature threshold (°C)
#' @param Tu The upper temperature threshold (°C)
#' @param a A parameter controlling the height of the curve
#' @param b A parameter controlling skew of the curve
#' @param c A parameter controlling concavity of the curve
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   analytis_allahyari_development(
#'     t  = 0:35,
#'     T0 = 5,
#'     Tu = 33,
#'     a  = 5,
#'     b  = 5,
#'     c  = 1
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
analytis_allahyari_development <- function(t, T0, Tu, a, b, c, ...) {
  delta <- (t - T0) / (Tu - T0)
  pmax(0, ifelse((t > Tu) | (t < T0), 0, a * delta ^ b * (1 - delta ^ c)))
}


#' Beta development model
#'
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520).
#'
#' @param t The temperature (°C)
#' @param Rm The maximum development rate
#' @param T0 The lower temperature threshold (°C)
#' @param Tm The optimum temperature (°C)
#' @param Tu The upper temperature threshold (°C)
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   beta_development(
#'     t  = 0:35,
#'     Rm = 0.2,
#'     T0 = 5,
#'     Tm = 28,
#'     Tu = 33
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
beta_development <- function(t, Rm, T0, Tm, Tu, ...) {
  pmax(0, ifelse((t > Tu) | (t < T0), 0, Rm * ((Tu - t) /
    (Tu - Tm)) * ((t - T0) / (Tm - T0))^((Tm - T0) / (Tu - Tm))))
}


#' Briere 1 development model
#'
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520),
#' Briere et al. (1999, Environ. Entomol. 28:22-29).
#'
#' @param t The temperature (°C)
#' @param T0 The lower temperature threshold (°C)
#' @param Tu The upper temperature threshold (°C)
#' @param a A parameter controlling the height of the curve
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   briere1_development(
#'     t  = 0:35,
#'     T0 = 5,
#'     Tu = 33,
#'     a  = 0.0001
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
briere1_development <- function(t, T0, Tu, a, ...) {
  pmax(0, ifelse((t > Tu) | (t < T0), 0, a * t * (t - T0) * sqrt(Tu - t)))
}


#' Briere 2 development model
#'
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520),
#' Briere et al. (1999, Environ. Entomol. 28:22-29).
#'
#' @param t The temperature (°C)
#' @param T0 The lower temperature threshold (°C)
#' @param Tu The upper temperature threshold (°C)
#' @param a A parameter controlling the height of the curve
#' @param b A parameter controlling the shape of the curve
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   briere2_development(
#'     t  = 0:35,
#'     T0 = 5,
#'     Tu = 33,
#'     a  = 0.0001,
#'     b  = 4
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
briere2_development <- function(t, T0, Tu, a, b, ...) {
  pmax(0, ifelse((t > Tu) | (t < T0), 0, a * t * (t - T0) * (Tu - t)^(1 / b)))
}


#' Calendar development model
#'
#' Models development rate R (/d) as a simple accumulation of time.
#'
#' @param t The temperature (°C) (NB not used)
#' @param Q The number of days required to complete the life stage
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   1:30,
#'   calendar_development(1:30),
#'   type = "l",
#'   xlab = "Time required Q (d)",
#'   ylab = "Development rate R (/d)"
#' )
#'
calendar_development <- function(Q, ...) {
  return(ifelse(Q <= 0, 1, 1 / Q))
}


#' Exponential development model
#'
#' @param t The temperature (°C)
#' @param sl A slope parameter
#' @param sp A spread parameter
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   exponential_development(
#'     t  = 0:35,
#'     sl = 0.01,
#'     sp = 0.1
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
exponential_development <- function(t, sl, sp, ...) {
  pmax(0, sl * exp(sp * t))
}


#' Exponential base development model
#'
#' @param t The temperature (°C)
#' @param T0 The base temperature for development (°C)
#' @param sl A slope parameter
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   expbase_development(
#'     t  = 0:35,
#'     T0 = 10,
#'     sl = 0.01
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
expbase_development <- function(t, T0, sl, ...) {
  pmax(0, exp(sl * (t - T0)) - 1)
}


#' Gaussian development model
#'
#' Also known as the Pradhan-Taylor model.
#' Here it has been modified to accommodate biologically meaningful input
#' parameters.
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520),
#' Pradhan (1945, Proc. Nat. Inst. Sci. India 11:73-80),
#' Han et al (2000, Annals Entomol. Soc. Am. 93:536-540),
#' Walgama & Zalucki (2006, Insect Sci. 13:109-118).
#'
#' @param t The temperature (°C)
#' @param Rm The maximum development rate
#' @param T0 The approximate lower threshold temperature (°C) where R = 5% of Rm
#' @param Tm The optimum temperature (°C)
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   gaussian_development(
#'     t  = 0:35,
#'     Rm = 0.2,
#'     T0 = 10,
#'     Tm = 30
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
gaussian_development <- function(t, Rm, T0, Tm, ...) {
  pmax(0, Rm * exp(-3 * ((t - Tm) / (T0 - Tm)) ^ 2))
}


#' Lactin 1 development model
#'
#' A modified form of the Logan 1 model, to eliminate an unnecessary parameter.
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520),
#' Lactin et al. (1995, Env. Entomol. 24: 68-75).
#'
#' @param t The temperature (°C)
#' @param rho A constant defining the development rate at the optimal
#'   temperature
#' @param Tu The upper temperature threshold (°C)
#' @param delta A range over which physiological breakdown becomes the
#'   overriding influence
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   lactin1_development(
#'     t     = 0:35,
#'     rho   = 0.15,
#'     Tu    = 33,
#'     delta = 6.4
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
lactin1_development <- function(t, rho, Tu, delta, ...) {
  pmax(0, exp(rho * t) - exp(rho * Tu - (Tu - t) / delta))
}


#' Lactin 2 development model
#'
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520),
#' Lactin et al. (1995, Env. Entomol. 24: 68-75).
#'
#' @param t The temperature (°C)
#' @param rho A constant defining the development rate at the optimal
#'   temperature
#' @param Tu The upper temperature threshold (°C)
#' @param delta A range over which physiological breakdown becomes the
#'   overriding influence
#' @param lambda A parameter that forces a low temperature threshold
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   lactin2_development(
#'     t      = 0:35,
#'     rho    = 0.15,
#'     Tu     = 33,
#'     delta  = 6.4,
#'     lambda = -0.3
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
lactin2_development <- function(t, rho, Tu, delta, lambda, ...) {
  pmax(0, exp(rho * t) - exp(rho * Tu - (Tu - t) / delta) + lambda)
}


#' Linear development model
#'
#' Models linear insect development rate as a function of temperature.
#'
#' @param t The temperature (°C)
#' @param b The base temperature for development (°C)
#' @param q The total day-degree requirement (°d)
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   linear_development(
#'     t = 0:35,
#'     b = 10,
#'     q = 100
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
linear_development <- function(t, b, q, ...) {
  pmax(0, t - b) / q
}


#' Logan 1 development model
#'
#' Also known as the Logan-6 model.
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520),
#' Logan et al. (1976, Env. Entomol. 5:1133-1140),
#' Logan (1988, Env. Entomol. 17: 359-376),
#' Lactin et al. (1995, Env. Entomol. 24: 68-75).
#'
#' @param t The temperature (°C)
#' @param psi A scalar for the development rate
#' @param rho A constant defining the development rate at the optimal
#'   temperature
#' @param Tu The upper temperature threshold (°C)
#' @param delta A temperature range over which physiological breakdown becomes
#'   the overriding influence (°C)
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   logan1_development(
#'     t     = 0:35,
#'     psi   = 0.05,
#'     rho   = 0.19,
#'     Tu    = 33,
#'     delta = 5
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
logan1_development <- function(t, psi, rho, Tu, delta, ...) {
  pmax(0, psi * (exp(rho * t) - exp(rho * Tu - (Tu - t) / delta)))
}


#' Logan 2 development model
#'
#' Also known as the Logan-10 model.
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520),
#' Logan et al. (1976, Env. Entomol. 5:1133-1140),
#' Logan (1988, Env. Entomol. 17: 359-376).
#'
#' @param t The temperature (°C)
#' @param a A scalar for the development rate
#' @param K An empirical constant
#' @param rho A constant defining the development rate at the optimal
#'   temperature
#' @param Tu The upper temperature threshold (°C)
#' @param delta The temperature range over which physiological breakdown becomes
#'   the overriding influence (°C)
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   logan2_development(
#'     t     = 0:35,
#'     a     = 0.5,
#'     K     = 50,
#'     rho   = 0.19,
#'     Tu    = 33,
#'     delta = 2
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
logan2_development <- function(t, a, K, rho, Tu, delta, ...) {
  pmax(0, a * (1 / (1 + K * exp(-rho * t))) - exp(-(Tu - t) / delta))
}


#' Sigmoid development model
#'
#' Also known as the Davidson or logistic model.
#' Here it has been modified to accommodate biologically meaningful input
#'   parameters.
#' See Mirhosseini et al. (2017, Annals Entomol. Soc. Am. 110:507-520),
#' Casagrande et al. (1987, Environ. Entomol. 16:556-562).
#'
#' @param t The temperature (°C)
#' @param Rm The upper limit of the sigmoid line (/d)
#' @param T0 The approximate lower threshold temperature (°C) where R = 5% of Rm
#' @param Tm The approximate optimal temperature (°C) where R = 95% of Rm
#' @param ... Unused variables
#' @returns Proportion of total developmental requirement completed
#' @export
#' @examples
#' plot(
#'   0:35,
#'   sigmoid_development(
#'     t  = 0:35,
#'     Rm = 0.1,
#'     T0 = 10,
#'     Tm = 30
#'   ),
#'   type = "l",
#'   xlab = "Temperature T (°C)",
#'   ylab = "Development rate R (/d)"
#' )
#'
sigmoid_development <- function(t, Rm, T0, Tm, ...) {
  b <- 6 / (Tm - T0)
  a <- b * T0 + 3
  pmax(0, Rm / (1 + exp(a - b * t)))
}



