# conventional order of the function parameters:
#
#      x    (function argument),
#      h    (bandwidth),
# Kernel    (kernel function, formerly known as K),
#   data    (sample data, formerly known as X),
# maxEval   (maximum number of function evaluations for cubintegrate)
#  n_obs    (number of observations, formerly known as N),
# bandwidths    (the set of bandwidths to test),
#      c    (calibration constant to weigh the variance term,
#            in the estimator for the bias term)
#      v    (calibration constant to weigh the variance term,
#            formerly known as x)

#' Double kernel estimator.
#'
#' @param h A double vector of length 1. A bandwidth.
#' @param h_prime A double vector of length 1. Another bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @return A function. The double kernel estimator.
get_double_kernel_estimator <- function(h, h_prime, Kernel, data, maxEval) {
  kde_h <- get_kde(h, Kernel, data)
  Kernel_h_prime <- function(x) {
    p_sapply(x, scaled_kernel, h_prime, Kernel)
  }
  convolution(Kernel_h_prime, kde_h, maxEval)
}

#' Estimator for the Bias Term.
#'
#' B_hat
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @param bandwidths A double vector. The set of h_prime bandwidths to test.
#' @param c A double vector of length 1. A calibration constant.
#' @return A double vector of length 1.
est_bias_GL <- function(h, Kernel, data, maxEval, bandwidths, c) {
  n_obs <- length(data)
  max(
    p_sapply(bandwidths, function(h_prime) {
      kde_h_prime <- get_kde(h_prime, Kernel, data)
      double_kernel_estimator <- get_double_kernel_estimator(h, h_prime, Kernel, data, maxEval)
      L2norm_squared(function(x) {
        kde_h_prime(x) - double_kernel_estimator(x)
      }, maxEval) -
        c * est_variance_GL(h_prime, Kernel, maxEval, n_obs)
    }
    ), 0)
}

#' Estimator for the Variance Term.
#'
#' V_hat
#'
#' @param h A double vector of bandwidths.
#' @param Kernel A real function. The kernel.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @param n_obs A double vector of length 1. The number of observations.
#' @return A double vector.
est_variance_GL <- function(h, Kernel, maxEval, n_obs) {
  L2norm_squared(Kernel, maxEval) / (n_obs * h)
}

#' Estimator for the Risk.
#'
#' Risk_hat
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @param bandwidths A double vector. The set of h_prime bandwidths to test.
#' @param c A double vector of length 1. A calibration constant for weighing the variance term inside the bias term.
#' @param v A double vector of length 1. A calibration constant for weighing the variance term.
#' @return A double vector of length 1.
est_risk_GL <- function(h, Kernel, data, maxEval, bandwidths, c, v) {
  n_obs <- length(data)
  est_bias_GL(h, Kernel, data, maxEval, bandwidths, c) +
    v * est_variance_GL(h, Kernel, maxEval, n_obs)
}

#' Optimisation criterion for bandwidth selection using the Goldenshluger-Lepski method.
#'
#' The Estimator for the Risk with all parameters fixed but the bandwidth h.
#' Minimise it to find the optimal value for h.
#'
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param bandwidths A double vector containing the bandwidths to try.
#' @param n_bandwidths A numeric vector of length 1. The number of bandwidths to test, only used if `bandwidths` is set to NULL.
#' @param c A double vector of length 1. A calibration constant.
#' @param v A double vector of length 1. A calibration constant.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @param lower A double vector of length 1. The lowest bandwidth to test.
#' @param upper A double vector of length 1. The highest bandwidth to test.
#' @return A vectorised single-parameter function. The Goldenshluger-Lepski bandwidth selection
#' optimisation criterion.
get_criterion_GL <- function(Kernel, data, maxEval = 1e6, bandwidths = NULL, n_bandwidths = 20, c = 1, v = 2, lower = NULL, upper = NULL) {
    critical_bandwidth <- 0.1
    if(is.null(bandwidths)) {
        stopifnot('Either bandwidths or both lower and upper must be set.' = is.double(lower) && is.double(upper))
        if(lower < critical_bandwidth) {
            warning(stringr::str_glue("You have chosen the lowest bandwidth smaller than {critical_bandwidth}. Taking value `lower = {critical_bandwidth}`."))
            lower <- critical_bandwidth
        }
        bandwidths <- seq(lower, upper, length.out = n_bandwidths)
    }
    else {
        if(min(bandwidths) < critical_bandwidth) {
            warning(stringr::str_glue("You have chosen the lowest bandwidth smaller than {critical_bandwidth}. Small values of the bandwidth may lead to wrong results."))
        }
    }
    force(Kernel)
    force(data)
    force(maxEval)
    force(bandwidths)
    force(c)
    force(v)
    function(h_vals) {
        p_sapply(h_vals, function(h) est_risk_GL(h, Kernel, data, maxEval, bandwidths, c, v))
    }
}
