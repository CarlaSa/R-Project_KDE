#' Calculate L2-norm squared of a real function.
#'
#' Calculate the L2-norm squared of a real function using integrate.
#'
#' @param f A real function
#' @return A double vector of length 1. The L2-norm squared of f.
#' @examples
#' L2norm_squared(kernels$triangular)
#' @export
L2norm_squared <- function(f) {
    integrate(function(u) f(u)^2, lower = -Inf, upper = Inf)$value
}

#' Calculate L2-norm of a real function.
#'
#' Calculate the L2-norm of a real function using integrate.
#'
#' @param f A real function
#' @return A double vector of length 1. The L2-norm of f.
#' @examples
#' L2norm(kernels$triangular)
#' @export
L2norm <- function(f) {
    sqrt(L2norm_squared(f))
}
