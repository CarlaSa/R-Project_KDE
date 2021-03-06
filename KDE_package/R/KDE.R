#' Scaled kernel.
#'
#' @param x A double vector. The argument.
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @return A double vector.
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
kde <- function(x, h, Kernel, data) {
    sum(scaled_kernel(data-x, h, Kernel)) / length(data)
}

#' Get a fix KDE.
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A Kernel object, a real function or the name of a Kernel from the `kernels` list. The kernel.
#' @param data A double vector of the sample data to use.
#' @return A double vector of length 1.
#' @export
get_kde <- function(h, Kernel, data) {
    stopifnot("h needs to be a positive vector of length 1." = is.numeric(h) & (h>0) & length(h)==1)
    stopifnot("data should be numeric" = is.numeric(data))
    stopifnot("data should be vector" = is.vector(data))
    if(is.character(Kernel)) {
        stopifnot('Kernel not found in `kernels`. Provide a valid name or a function or Kernel object instead.' = Kernel %in% names(kernels))
        Kernel <- kernels[Kernel]
    }
    stopifnot('Kernel needs to be a Kernel object, a function or the name of a Kernel in `kernels`.' = is_Kernel(Kernel) || is.function(Kernel))
    
    function(x) {
        p_sapply(x, kde, h, Kernel, data)
    }
}
