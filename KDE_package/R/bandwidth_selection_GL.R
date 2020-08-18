# conventional order of the function parameters:
#
#      x    (function argument),
#      h    (bandwidth),
# Kernel    (kernel function, formerly known as K),
#   data    (sample data, formerly known as X),
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
#' @param data A double vector of the sample data to use.
#' @return A function. The double kernel estimator.
get_double_kernel_estimator <- function(h, h_prime, Kernel, data) {
  kde_h <- get_kde(h, Kernel, data)
  Kernel_h_prime <- function(x) {
    sapply(x, scaled_kernel, h, Kernel)
  }
  convolution(Kernel_h_prime, kde_h)
}

#' Estimator for the Bias Term.
#'
#' B_hat
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param bandwidths A double vector. The set of h_prime bandwidths to test.
#' @param c A double vector of length 1. A calibration constant.
#' @param v A double vector of length 1. A calibration constant.
#' @return A double vector of length 1.
est_bias <- function(h, Kernel, data, bandwidths, c, v) {
  n_obs <- length(data)
  max(
    sapply(bandwidths, function(h_prime) {
      kde_h_prime <- get_kde(h_prime, Kernel, data)
      double_kernel_estimator <- get_double_kernel_estimator(h, h_prime, Kernel, data)
      L2norm_squared(function(x) {
        kde_h_prime(x) - double_kernel_estimator(x)
      }) -
        c * est_variance(h_prime, Kernel, n_obs, v)
    }
    ))
}

#' Estimator for the Variance Term.
#'
#' V_hat
#'source('R/bandwidth_selection.R', chdir=T)
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
#' @param bandwidths A double vector. The set of h_prime bandwidths to test.
#' @param m A double vector of length 1. The smallest bandwidth.
#' @param v A double vector of length 1. A calibration constant for weighing the variance term.
#' @return A double vector of length 1.
est_risk <- function(h, Kernel, data, bandwidths, c, v) {
  n_obs <- length(data)
  est_bias(h, Kernel, data, bandwidths, c, v) +
    est_variance(h, Kernel, n_obs, v)
}

#' Optimisation criterion for bandwidth selection using the Goldenshluger-Lepski method.
#'
#' The Estimator for the Risk with all parameters fixed but the bandwidth h.
#' Minimise it to find the optimal value for h.
#'
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param bandwidths A double vector containing the bandwidths to try.
#' @param c A double vector of length 1. A calibration constant.
#' @param v A double vector of length 1. A calibration constant.
#' @return A vectorised single-parameter function. The Goldenshluger-Lepski bandwidth selection
#' optimisation criterion.
get_criterion_GL <- function(Kernel, data, bandwidths, c = 1, v = 1) {
  force(Kernel)
  force(data)
  force(bandwidths)
  force(c)
  force(v)
  function(h_vals) {
    sapply(h_vals, function(h) est_risk(h, Kernel, data, bandwidths, c, v))
  }
}
