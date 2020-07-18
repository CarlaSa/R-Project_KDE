source('kernels.R')
source('rejection_sample.R')
source('L2norm.R')

#' Scaled kernel.
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param t A double vector. The argument.
#' @param Kernel A real function. The kernel.
#' @return A double vector.
K_h <- function(h, t, Kernel) {
    Kernel(t / h) / h
}

#' Kernel density estimator.
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param data A double vector of the sample data to use.
#' @param v A double vector of length 1. The argument.
#' @param Kernel A real function. The kernel.
#' @return A double vector of length 1.
f_hat <- function(h, data, v, Kernel) {
    sum(K_h(h, data-v, Kernel)) / length(data)
}

#' Estimator for the Bias Term.
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param m A double vector of length 1. The smallest bandwidth.
#' @param data A double vector of the sample data to use.
#' @param Kernel A real function. The kernel.
#' @return A double vector of length 1.
B_hat <- function(h, m, data, Kernel) {
    # comparison to overfitting
    L2norm_squared(sapplify(function(v) {
                                f_hat(h, data, v, Kernel) -
                                    f_hat(m, data, v, Kernel)
})) -
             # penalized
             L2norm_squared(sapplify(function(t) {
                                         K_h(h, t, Kernel) -
                                             K_h(m, t, Kernel)
})) / length(data)
}

#' Estimator for the Variance Term.
#'
#' @param h A double vector of bandwidths.
#' @param x A double vector of length 1. A calibration constant.
#' @param n_obs A double vector of length 1. The number of observations.
#' @param Kernel A real function. The kernel.
#' @return A double vector.
V_hat <- function(h, x, n_obs, Kernel) {
    x * L2norm_squared(Kernel) / (n_obs * h)
}


#' Estimator for the Risk.
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param m A double vector of length 1. The smallest bandwidth.
#' @param data A double vector of the sample data to use.
#' @param x A double vector of length 1. A calibration constant.
#' @param Kernel A real function. The kernel.
#' @return A double vector of length 1.
Risk_hat <- function(h, m, data, x, Kernel) {
    n_obs <- length(data)
    B_hat(h, m, data, Kernel) +
        V_hat(h, x, n_obs, Kernel)
}

#' Vectorise a single-parameter function using sapply.
#'
#' @param f A single-parameter function.
#' @return A vectorised single-parameter function.
sapplify <- function(f) {
    function(a) {
        sapply(a, f)
    }
}

#' Optimisation criterion for bandwidth selection using PCO.
#'
#' The Estimator for the Risk with all parameters fixed but the bandwidth h.
#' Minimise it to find the optimal value for h.
#'
#' @param m A double vector of length 1. The smallest bandwidth.
#' @param data A double vector of the sample data to use.
#' @param x A double vector of length 1. A calibration constant.
#' @param Kernel A real function. The kernel.
#' @return A vectorised single-parameter function. The PCO bandwidth selection
#' optimisation criterion.
criterion <- function(m, data, x, Kernel) {
    force(m)
    force(x)
    force(Kernel)
    force(data)
    function(bandwidths) {
        sapply(bandwidths, function(h) Risk_hat(h, m, data, x, Kernel))
    }
}

#' Bandwidth selection using PCO.
#'
#' Find the optimal value for the bandwidth h.
#'
#' @param Kernel A real function. The kernel.
#' @param n_obs A double vector of length 1. The number of observations.
#' @param bandwidths A double vector containing the bandwidths to try.
#' @param x A double vector of length 1. A calibration constant.
#' @param data A double vector of the sample data to use.
#' @return A double vector of length 1. The optimal bandwidth.
#' @export
bws_PCO <- function(
                    Kernel = kernels$gaussian,
                    n_obs = 100,
                    bandwidths = seq(from = 0.01, to = 1, length.out = 100),
                    x = 1,
                    data = rejection_sample(n_obs, Kernel)
                    ) {
    stopifnot('n_obs must equal length(data)' = n_obs == length(data))
    risks <- criterion(min(bandwidths), data, x, Kernel)(bandwidths)
    bandwidths[which.min(risks)]
}
