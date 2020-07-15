source('kernels.R')

#' Rejection sampling.
#'
#' Generate observations from an unknown distribution with a known density
#' function using the rejection sampling method.
#'
#' @param n_obs An integer vector of length 1. The number of observations.
#' @param Kernel A probability density function to generate observations from.
#' @param Y A helper distribution function.
#' @param g A real function. The probability density function of the Y
#' distribution.
#' @param n_iter An integer vector of length 1. A calibrating constant. The
#' algorithm will take an average of n_iter iterations to obtain a sample.
#' @return A double vector of length n_obs. The observations.
rejection_sample <- function(n_obs,
                  Kernel,
                  Y = function() rnorm(1, 0, 1),
                  g = kernels$gaussian,
                  n_iter = 10
) {
    sapply(seq(n_obs), function(i) {
               while (TRUE) {
                   y <- Y()
                   u <- runif(1, 0, 1)
                   if (u < Kernel(y) / n_iter / g(y))
                       return(y)
               }
    })
}
