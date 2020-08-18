source('tools.R')

#' Calculate the convolution product of two real functions.
#'
#' Calculate the convolution product of two real functions using integrate.
#'
#' @param f A real function
#' @param g A real function
#' @return A function. The convolution product of f, g.
#' @export
convolution <- function(f, g) {
    sapplify(function(x) {
        integrate(function(u) f(u) * g(x-u), lower = -Inf, upper = Inf)$value
    })
}
