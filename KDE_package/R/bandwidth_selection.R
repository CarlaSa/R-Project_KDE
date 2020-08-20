#' Bandwidth selection using optimise.
#'
#' Find the optimal value for the bandwidth h.
#'
#' @param criterion_method A string. Keyword of the bandwidth selection
#'   algorithm to use. Implemented are: "CV" the Cross Validation method "GL"
#'   the Goldenshluger-Lepski method "PCO" the Penalised Comparison to
#'   Overfitting method
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param lower A double vector of length 1. The lowest bandwidth to test.
#' @param upper A double vector of length 1. The greatest bandwidth to test.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @param ... Optional arguments for the criterion function.
#' @return A double vector of length 1. The optimal bandwidth.
#' @details Optional arguments and defaults for PCO: \cr \code{v = 1}, a
#'   calibration constant for the variance term. \cr \cr Optional arguments and
#'   defaults for GL: \cr \code{bandwidths = NULL}, a sequence of bandwidths to
#'   use as h_prime. Otherwise: \cr \code{n_bandwidths = 20}, if no set
#'   bandwidths are provided, a sequence of this length is generated between
#'   \code{lower} and \code{upper}. \cr \code{c = 1}, the calibration constant
#'   in the bias term. \cr \code{v = 2}, a calibration constant for the variance
#'   term. \cr Note that in the GL method, \code{lower} is set to a minimum
#'   value of \code{0.1}, a critical bandwidth below which the implementation is
#'   no longer stable.
#' @export
bandwidth_selection <- function(criterion_method,
                                Kernel = kernels$gaussian,
                                data,
                                maxEval = 1e3,
                                lower = 1e-3,
                                upper = 1e0,
                                ...) {
  # check inputs
  bws_methods <- bandwidth_selection_criteria()
  stopifnot("criterion_method must be `GL`,`PCO` or `CV`" = criterion_method %in% names(bws_methods))
  stopifnot("Kernel not valid" = is_Kernel(Kernel))
  stopifnot("data should be numeric" = is.numeric(data))
  stopifnot("data should be vector" = is.vector(data))
  stopifnot("lower should be a positive number" = is.numeric(lower) & length(lower) == 1 & lower>0)
  stopifnot("upper should be a positive number" = is.numeric(upper) & length(upper) == 1 & upper>0)
  stopifnot("lower should be smaller than upper" = lower<upper)
  stopifnot("maxEval should be a positive number" = is.numeric(maxEval) & length(maxEval) == 1 & maxEval > 0)

  criterion_getter <- bws_methods[[criterion_method]]
  if ('lower' %in% formalArgs(criterion_getter)) {
    if ('upper' %in% formalArgs(criterion_getter)) {
      criterion <-
        criterion_getter(Kernel, data, maxEval, lower = lower, upper = upper, ...)
    }
    else {
      criterion <- criterion_getter(Kernel, data, maxEval, lower = lower, ...)
    }
  }
  else {
    criterion <- criterion_getter(Kernel, data, maxEval, ...)
  }
  optimise(criterion, lower = lower, upper = upper)$minimum
}

#' Getters for the bandwidth selection criteria.
#'
#' @return A list of functions. The list of bandwidth selection criteria provided by the package.
#' @export
bandwidth_selection_criteria <- function() {
  list(CV = get_criterion_CV,
       GL = get_criterion_GL,
       PCO = get_criterion_PCO)
}
