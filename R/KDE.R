#' Scaled kernel.
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param x A double vector. The argument.
#' @param Kernel A real function. The kernel.
#' @return A double vector.
K_h <- function(h, x, Kernel) {
    Kernel(x / h) / h
}

#' Kernel density estimator.
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param data A double vector of the sample data to use.
#' @param x A double vector of length 1. The argument.
#' @param Kernel A real function. The kernel.
#' @return A double vector of length 1.
kde <- function(h, data, x, Kernel) {
    sum(K_h(h, data-x, Kernel)) / length(data)
}

#' Get a fix KDE.
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @return A double vector of length 1.
get_kde <- function(h, Kernel, data) {
    function(v) {
        sapply(v, function(x)
            sum(K_h(h, data-x, Kernel)) / length(data)
            )
    }
}