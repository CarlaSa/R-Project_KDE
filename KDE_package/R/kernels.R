#' Constructor for a Kernel object.
#'
#' @param func A function. The kernel function.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @return A Kernel object.
Kernel <- function(func, maxEval = 1e6) {
    stopifnot("func must be a function"  = is.function(func))
    stopifnot("maxEval must have positive numerical value" = is.numeric(maxEval) & maxEval>0)
    K <- func
    attr(K, 'L2norm_squared') <- L2norm_squared(K, maxEval)
    attr(K, 'L2norm') <- sqrt(attr(K, 'L2norm_squared'))
    # probably TODO: number of subdivisions required
    class(K) <- 'Kernel'
    K
}

#' Validation function for Kernel objects.
#'
#' @param object Any object. This object is validated to be a Kernel.
#' @param check_values TRUE or FALSE. If set to TRUE, the L2norm_squared and L2norm values are
#' checked and the integral of the Kernel is calculated. The return value will be TRUE only if
#' the integral equals 1.
#' @param tolerance_rel A double vector of length 1. The relative tolerance for any equality checks.
#' This is only needed when check_values is set to TRUE.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @return A logical.
is_Kernel <- function(object, check_values = FALSE, tolerance_rel = 1e-12, maxEval = 1e6) {
    class(object) == 'Kernel' &&
        is.function(object) &&
        is.double(attr(object, 'L2norm')) &&
        is.double(attr(object, 'L2norm_squared')) &&
        length(attr(object, 'L2norm')) == 1 &&
        length(attr(object, 'L2norm_squared')) == 1 &&
        (!check_values || (
            abs(attr(object, 'L2norm_squared') / attr(object, 'L2norm')^2 - 1) < tolerance_rel &&
                abs(L2norm_squared(object) / attr(object, 'L2norm_squared') - 1) < tolerance_rel &&
                {
                    integral <- integrate(object, lower = -Inf, upper = Inf)
                    abs(integral$value - 1) < integral$abs.error
                }
        ))
}

#' Some Kernels.
#'
#' @export
kernels <- list(
             gaussian = Kernel(function(u) {
                 1 / (sqrt(2*pi)) * exp(-u^2 / 2)
             }),
             rectangular = Kernel(function(u) {
                 1/2 * (abs(u) <= 1)
             }),
             triangular = Kernel(function(u) {
                 (1 - abs(u)) * (abs(u) <= 1)
             }),
             epanechnikov = Kernel(function(u) {
                 3/4 * (1 - u^2) * (abs(u) <= 1)
             }),
             biweight = Kernel(function(u) {
                 15/16 * (1 - u^2)^2 * (abs(u) <= 1)
             }),
             silverman = Kernel(function(u) {
                 1/2 * exp(-abs(u)/sqrt(2)) *
                     sin(abs(u)/sqrt(2) + pi/4)
             })
)
kernels$parabolic <- kernels$epanechnikov
