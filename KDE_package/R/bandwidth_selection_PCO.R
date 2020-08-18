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
#' @return A double vector of length 1.
est_bias <- function(h, Kernel, data, m) {
  # comparison to overfitting
  L2norm_squared(sapplify(function(x) {
    kde(x, h, Kernel, data) -
      kde(x, m, Kernel, data)
  })) -
    # penalized
    L2norm_squared(sapplify(function(x) {
      scaled_kernel(x, h, Kernel) -
        scaled_kernel(x, m, Kernel)
    })) / length(data)
}

#' Estimator for the Variance Term.
#'
#' V_hat
#'
#' @param h A double vector of bandwidths.
#' @param Kernel A real function. The kernel.
#' @param n_obs A double vector of length 1. The number of observations.
#' @param v A double vector of length 1. A calibration constant.
#' @return A double vector.
est_variance <- function(h, Kernel, n_obs, v) {
  v * L2norm_squared(Kernel) / (n_obs * h)
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
#' @return A double vector of length 1.
est_risk <- function(h, Kernel, data, m, v) {
  n_obs <- length(data)
  est_bias(h, Kernel, data, m) +
    est_variance(h, Kernel, n_obs, v)
}

#' Optimisation criterion for bandwidth selection using PCO.
#'
#' The Estimator for the Risk with all parameters fixed but the bandwidth h.
#' Minimise it to find the optimal value for h.
#'
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param m A double vector of length 1. The smallest bandwidth.
#' @param v A double vector of length 1. A calibration constant.
#' @return A vectorised single-parameter function. The PCO bandwidth selection
#' optimisation criterion.
#' @export
get_criterion_PCO <- function(Kernel, data, m, v = 1) {
  force(Kernel)
  force(data)
  force(m)
  force(v)
  function(bandwidths) {
    sapply(bandwidths, function(h) est_risk(h, Kernel, data, m, v))
  }
}

