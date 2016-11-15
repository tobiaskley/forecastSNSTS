#' @include forecastSNSTS-package.R
NULL

################################################################################
#' Forecasting of Stationary and Non-Stationary Time Series
#'
#' Methods to compute linear \eqn{h}-step prediction coefficients based on
#' localised and iterated Yule-Walker estimates and empirical mean square
#' prediction errors for the resulting predictors.
#'
#' @details
#'  \tabular{ll}{
#'    \cr Package: \tab forecastSNSTS
#'    \cr Type:    \tab Package
#'    \cr Version: \tab 1.0-0.9000
#'    \cr Date:    \tab 2016-11-15
#'    \cr License: \tab GPL (>= 2)
#'  }
#'
#' @section Contents:
#' The core functionality of this R package is accessable via the function
#' \code{\link{predCoef}}, which is used to compute the linear prediction
#' coefficients, and the function \code{\link{MSPE}}, which is used to compute
#' the empirical mean squared prediciton erros.
#' 
#' The function \code{\link{tvARMA}} can be used to simulate time varying
#' ARMA(p,q) time series.
#'
#' @name forecastSNSTS-package
#' @aliases forecastSNSTS
#' @docType package
#' @author Tobias Kley
#'
#' @useDynLib forecastSNSTS
#' @importFrom Rcpp sourceCpp

#'
#' @references
#' Kley, T., Preuss, P. & Fryzlewicz, P. (2016).
#' Predictive, finite-sample model choice for time series under stationarity and non-stationarity.
#' [cf. \url{https://arxiv.org/abs/1611.04460}]
#'
NULL

# Taken from quantreg-package and adapted.
".onAttach" <- function(lib, pkg) {
  if(interactive() || getOption("verbose"))
    packageStartupMessage("Package forecastSNSTS loaded.\n     To cite, see citation(\"forecastSNSTS\").\n     For demos, see demo(package = \"forecastSNSTS\").")
}
