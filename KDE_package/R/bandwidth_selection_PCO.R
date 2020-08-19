# conventional order of the function parameters:
#
#      x    (function argument),
#      h    (bandwidth),
# Kernel    (kernel function, formerly known as K),
#   data    (sample data, formerly known as X),
#  n_obs    (number of observations, formerly known as N),
#      m    (minimum bandwidth from the set of bandwidths to test),
#      v    (calibration constant to weigh the variance term,
#            formerly known as x)

#' Estimator for the Bias Term
#'
#' B_hat
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param m A double vector of length 1. The smallest bandwidth.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @return A double vector of length 1.
est_bias_PCO <- function(h, Kernel, data, m, maxEval) {
  # comparison to overfitting
  L2norm_squared(sapplify(function(x) {
    kde(x, h, Kernel, data) -
      kde(x, m, Kernel, data)
  }), maxEval) -
    # penalized
    L2norm_squared(sapplify(function(x) {
      scaled_kernel(x, h, Kernel) -
        scaled_kernel(x, m, Kernel)
    }), maxEval) / length(data)
}

#' Estimator for the Variance Term.
#'
#' V_hat
#'
#' @param h A double vector of bandwidths.
#' @param Kernel A real function. The kernel.
#' @param n_obs A double vector of length 1. The number of observations.
#' @param v A double vector of length 1. A calibration constant.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @return A double vector.
est_variance_PCO <- function(h, Kernel, n_obs, v, maxEval) {
  v * L2norm_squared(Kernel, maxEval) / (n_obs * h)
}

#' Estimator for the Risk.
#'
#' Risk_hat
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param m A double vector of length 1. The smallest bandwidth.
#' @param v A double vector of length 1. A calibration constant for weighing the variance term.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @return A double vector of length 1.
est_risk_PCO <- function(h, Kernel, data, m, v, maxEval) {
  n_obs <- length(data)
  est_bias_PCO(h, Kernel, data, m, maxEval) +
    est_variance_PCO(h, Kernel, n_obs, v, maxEval)
}

#' Optimisation criterion for bandwidth selection using PCO.
#'
#' The Estimator for the Risk with all parameters fixed but the bandwidth h.
#' Minimise it to find the optimal value for h.
#'
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param lower A double vector of length 1. The lowest bandwidth, used for comparison to overfitting.
#' @param v A double vector of length 1. A calibration constant.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @return A vectorised single-parameter function. The PCO bandwidth selection
#' optimisation criterion.
#' @export
get_criterion_PCO <- function(Kernel, data, lower, v = 1, maxEval = 1e6) {
  force(Kernel)
  force(data)
  force(lower)
  force(v)
  force(maxEval)
  function(bandwidths) {
    sapply(bandwidths, function(h) est_risk_PCO(h, Kernel, data, lower, v, maxEval))
  }
}

