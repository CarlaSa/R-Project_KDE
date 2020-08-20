#' Constructor of a helper (distribution) object for rejection sampling.
#'
#' A helper distribution is a distribution function which has an
#' attribute 'density' which is a probability density function.
#' It is used for rejection sampling.
#'
#' @param distribution A function. A distribution function.
#' Must not have any non-optional parameters. Must return a double vector
#' of length 1 (single sample at once) when called without parameters.
#' @param density A function. The corresponding probability density
#' function to the distribution. Must accept a double vector as a
#' parameter. Must return a double vector of the same length.
#' (Vectorisation is not required.)
#' @return A helper object composed of the distribution and the density functions.
helper <- function(distribution, density) {
    stopifnot('distribution must be a function' = is.function(distribution))
    stopifnot('density must be a function' = is.function(density))
    H <- distribution
    attr(H, 'density') <- density
    class(H) <- 'helper'
    H
}

#' Validation function for helper (distribution) objects for rejection sampling.
#'
#' @param object An object. This object is validated to be a helper object.
#' @return A logical value.
is_helper <- function(object) {
    if(!(class(object) == 'helper' &&
            is.function(object) &&
            is.function(attr(object, 'density'))))
        return(FALSE)
    distribution <- object
    sample <- distribution()
    density <- attr(object, 'density')
    density_at_sample <- density(sample)
    is.atomic(sample) &&
        is.double(sample) &&
        length(sample) == 1 &&
        density_at_sample > 0 &&
        is.atomic(density_at_sample) &&
        is.double(density_at_sample) &&
        length(density_at_sample) == 1
}

#' Helper distributions for rejection sampling.
#'
#' @export
helpers <- list(
     uniform = helper(function() runif(1, -1, 1), kernels$rectangular),
     normal = helper(function() rnorm(1, 0, 1), kernels$gaussian)
)

#' Rejection sampling.
#'
#' Generate observations from an unknown distribution with a known density
#' function using the rejection sampling method.
#'
#' @param n_obs A positive integer vector of length 1. The number of observations.
#' @param f A probability density function to simulate observations from.
#' @param helper A helper (distribution) object.
#' @param n_iter An integer vector of length 1. A calibrating constant. The
#' algorithm will take an average of n_iter iterations to obtain a sample.
#' @return A double vector of length n_obs. The observations.
#' @export
rejection_sample <- function(n_obs,
                             f,
                             helper = helpers$normal,
                             n_iter = 10
                            ) {
    if(is.na(n_obs)) stop("n_obs is NA. Please set n_obs to positive number.")
    stopifnot("n_obs must be a positive numeric vector length 1." = is.numeric(n_obs) & length(n_obs)==1 & n_obs>0)
    stopifnot("f must be a valid pdf function" = is.function(f) & abs(integrate(f, -Inf, Inf)$value - 1)<1e-3 & min(f(seq(-5, 5, length.out = 100)))>= 0)
    stopifnot("helper not valid. Options are: helpers$normal, helpers$uniform" = is_helper(helper))
    if(is.na(n_iter)) stop("n_iter is NA. Please set n_iter to positive number.")
    stopifnot("n_iter must be a positive numeric vector length 1." = is.numeric(n_iter) & length(n_iter)==1 & n_iter>0)
    helper_density <- attr(helper, 'density')
    p_sapply(seq(n_obs), function(i) {
        while (TRUE) {
            y <- helper()
            u <- runif(1, 0, 1)
            if (u < f(y) / n_iter / helper_density(y))
                return(y)
            }
    })
}
