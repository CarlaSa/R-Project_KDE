#' Scaled kernel.
#'
#' @param x A double vector. The argument.
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @return A double vector.
#' @export
scaled_kernel <- function(x, h, Kernel) {
    Kernel(x / h) / h
}

#' Kernel density estimator.
#'
#' @param x A double vector of length 1. The argument.
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @return A double vector of length 1.
#' @export
kde <- function(x, h, Kernel, data) {
    sum(scaled_kernel(data-x, h, Kernel)) / length(data)
}

#' Get a fix KDE.
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @return A double vector of length 1.
#' @export
get_kde <- function(h, Kernel, data) {
    function(v) {
        sapply(v, kde, h, Kernel, data)
    }
}