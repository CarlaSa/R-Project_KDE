source('kernels.R')

#' Rejection sampling.
#'
#' Generate observations from an unknown distribution with a known density
#' function using the rejection sampling method.
#'
#' @param n_obs An integer vector of length 1. The number of observations.
#' @param f A probability density function to simulate observations from.
#' @param helper_distribution A helper distribution function.
#' @param helper_density A real function. The probability density function of the Y
#' distribution.
#' @param n_iter An integer vector of length 1. A calibrating constant. The
#' algorithm will take an average of n_iter iterations to obtain a sample.
#' @return A double vector of length n_obs. The observations.
rejection_sample <- function(n_obs,
                             f,
                             helper_distribution = function() rnorm(1, 0, 1),
                             helper_density = kernels$gaussian,
                             n_iter = 10
                            ) {
    sapply(seq(n_obs), function(i) {
        while (TRUE) {
            y <- helper_distribution()
            u <- runif(1, 0, 1)
            if (u < f(y) / n_iter / helper_density(y))
                return(y)
            }
    })
}
