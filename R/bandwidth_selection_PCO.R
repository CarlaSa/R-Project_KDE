source('kernels.R')
source('L2norm.R')
source('KDE.R')
source('tools.R')

# Notation rule of thumb:
# arg. x, h, Kernel, data, n_obs, m, const. x

#' Estimator for the Bias Term.
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param m A double vector of length 1. The smallest bandwidth.
#' @return A double vector of length 1.
B_hat <- function(h, Kernel, data, m) {
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
#' @param h A double vector of bandwidths.
#' @param Kernel A real function. The kernel.
#' @param n_obs A double vector of length 1. The number of observations.
#' @param x A double vector of length 1. A calibration constant.
#' @return A double vector.
V_hat <- function(h, Kernel, n_obs, x) {
    x * L2norm_squared(Kernel) / (n_obs * h)
}

#' Estimator for the Risk.
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param m A double vector of length 1. The smallest bandwidth.
#' @param x A double vector of length 1. A calibration constant for the variance term.
#' @return A double vector of length 1.
Risk_hat <- function(h, Kernel, data, m, x) {
    n_obs <- length(data)
    B_hat(h, Kernel, data, m) +
        V_hat(h, Kernel, n_obs, x)
}

#' Optimisation criterion for bandwidth selection using PCO.
#'
#' The Estimator for the Risk with all parameters fixed but the bandwidth h.
#' Minimise it to find the optimal value for h.
#'
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param m A double vector of length 1. The smallest bandwidth.
#' @param x A double vector of length 1. A calibration constant.
#' @return A vectorised single-parameter function. The PCO bandwidth selection
#' optimisation criterion.
criterion <- function(Kernel, data, m, x) {
    force(Kernel)
    force(data)
    force(m)
    force(x)
    function(bandwidths) {
        sapply(bandwidths, function(h) Risk_hat(h, Kernel, data, m, x))
    }
}

#' Bandwidth selection using PCO.
#'
#' Find the optimal value for the bandwidth h.
#'
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param bandwidths A double vector containing the bandwidths to try.
#' @param x A double vector of length 1. A calibration constant.
#' @return A double vector of length 1. The optimal bandwidth.
#' @export
bws_PCO <- function(
                    Kernel = kernels$gaussian,
                    data = rejection_sample(1e4, Kernel),
                    bandwidths = seq(from = 0.01, to = 1, length.out = 100),
                    x = 1
                    ) {
    risks <- criterion(Kernel, data, min(bandwidths), x)(bandwidths)
    bandwidths[which.min(risks)]
}
