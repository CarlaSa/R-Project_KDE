# conventional order of the function parameters:
#
#      x    (function argument),
#      h    (bandwidth),
# Kernel    (kernel function, formerly known as K),
#   data    (sample data, formerly known as X),
# maxEval   (maximum number of function evaluations for cubintegrate)
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
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @param m A double vector of length 1. The smallest bandwidth.
#' @return A double vector of length 1.
est_bias_PCO <- function(h, Kernel, data, maxEval, m) {
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
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @param n_obs A double vector of length 1. The number of observations.
#' @param v A double vector of length 1. A calibration constant.
#' @return A double vector.
est_variance_PCO <- function(h, Kernel, maxEval, n_obs) {
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
#' @param m A double vector of length 1. The smallest bandwidth.
#' @param v A double vector of length 1. A calibration constant for weighing the variance term.
#' @return A double vector of length 1.
est_risk_PCO <- function(h, Kernel, data, maxEval, m, v) {
  n_obs <- length(data)
  est_bias_PCO(h, Kernel, data, maxEval, m) +
    v * est_variance_PCO(h, Kernel, maxEval, n_obs)
}

#' Optimisation criterion for bandwidth selection using PCO.
#'
#' The Estimator for the Risk with all parameters fixed but the bandwidth h.
#' Minimise it to find the optimal value for h.
#'
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @param lower A double vector of length 1. The lowest bandwidth, used for comparison to overfitting.
#' @param v A double vector of length 1. A calibration constant.
#' @return A vectorised single-parameter function. The PCO bandwidth selection
#' optimisation criterion.
get_criterion_PCO <- function(Kernel, data, maxEval = 1e6, lower, v = 1) {
  force(Kernel)
  force(data)
  force(maxEval)
  force(lower)
  force(v)
  function(bandwidths) {
    p_sapply(bandwidths, function(h) est_risk_PCO(h, Kernel, data, maxEval, lower, v))
  }
}

