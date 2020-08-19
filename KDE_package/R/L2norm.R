#' Calculate L2-norm squared of a real function.
#'
#' Calculate the L2-norm squared of a real function using integrate.
#'
#' @param f A real function
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @return A double vector of length 1. The L2-norm squared of f.
#' @examples
#' L2norm_squared(kernels$triangular)
L2norm_squared <- function(f, maxEval) {
    if(is_Kernel(f))
        return(attr(f, 'L2norm_squared'))
    #integrate(function(u) f(u)^2, lower = -Inf, upper = Inf)$value
    cubature::cubintegrate(function(u) f(u)^2, lower = -Inf, upper = Inf, method = "pcubature", maxEval=maxEval)$integral
}

#' Calculate L2-norm of a real function.
#'
#' Calculate the L2-norm of a real function using integrate.
#'
#' @param f A real function
#' @return A double vector of length 1. The L2-norm of f.
#' @examples
#' L2norm(kernels$triangular)
L2norm <- function(f) {
    if(is_Kernel(f))
        return(attr(f, 'L2norm_squared'))
    sqrt(L2norm_squared(f))
}
