#' Calculate the convolution product of two real functions.
#'
#' Calculate the convolution product of two real functions using integrate.
#'
#' @param f A real function
#' @param g A real function
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @return A function. The convolution product of f, g.
#' @export
convolution <- function(f, g, maxEval) {
    sapplify(function(x) {
        #integrate(function(u) f(u) * g(x-u), lower = -Inf, upper = Inf)$value
        cubintegrate(function(u) f(u) * g(x-u), lower = -Inf, upper = Inf, method = "pcubature", maxEval=maxEval)$integral
    })
}
