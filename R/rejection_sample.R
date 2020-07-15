source('kernels.R')

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
