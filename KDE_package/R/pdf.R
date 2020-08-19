# Some probability density function factories.

#' Get a Cauchy distribution function.
#'
#' @param x0 A double vector of length 1. The location parameter.
#' @param gamma A double vector of length 1. The scale parameter.
#' @return A vectorised function. A Cauchy distribution function.
#' @export
get_cauchy <- function(x0 = 0, gamma = 0.5) {
    function(x)
        1/(pi * gamma * (1 + ((x - x0) / gamma)^2))
}

#' Get a uniform distribution function.
#'
#' @param a A double vector of length 1. A location parameter.
#' @param b A double vector of length 1. A location parameter.
#' @return A vectorised function. A uniform distribution function.
#' @export
get_uniform <- function(a = -1, b = 1) {
    function(x)
        abs(1/(b-a)) * (a <= x & x <= b)
}

#' Get an exponential distribution function.
#'
#' @param lambda A double vector of length 1.
#' The rate (inverse scale) parameter.
#' @return A vectorised function. An exponential distribution function.
#' @export
get_exponential <- function(lambda = 1) {
    function(x)
        lambda * exp(-lambda * x) * (x >= 0)
}

#' Get a normal distribution function.
#'
#' @param mu A double vector of length 1. The mean.
#' @param sigma A double vector of length 1. The standard deviation.
#' @return A vectorised function. A normal distribution function.
#' @export
get_normal <- function(mu = 0, sigma = 1) {
    function(x)
        1/(sigma * sqrt(2 * pi)) * exp(-1/2 * ((x - mu)/sigma)^2)
}

#' Get a Laplace distribution function.
#'
#' @param mu A double vector of length 1. The mean.
#' @param b A double vector of length 1. The scale parameter.
#' @return A vectorised function. A Laplace distribution function.
#' @export
get_laplace <- function(mu = 0, b = 1) {
    function(x) {
        1/(2*b) * exp(-abs(x - mu)/b)
    }
}

#' Get a distribution function from a string containing a custom expression.
#'
#' @param str A character vector of lengh 1. A string containing a function expression using a numeric parameter named x. Must be a valid function body. The resulting function must be vectorised.
#' @return A vectorised function. A custom distribution function.
#' @export
get_custom <- function(str = 'abs(x)/4 * (abs(x) <= 2)') {
    function(x) eval(rlang::parse_expr(str))
}

#' Some probability density functions.
#' @export
pdfs <- list(
    Cauchy = get_cauchy(0, 0.5),
    Uniform = get_uniform(0, 1),
    Expo = get_exponential(1),
    Mix2Gauss = {
        .norm1 <- get_normal(0, 1)
        .norm2 <- get_normal(3, 9)
        function(x)
            (.norm1(x) + .norm2(x)) / 2
    }
)

#' All probability density functions available in one list.
#' @export
pdf_factories <- list(
    Cauchy = get_cauchy,
    uniform = get_uniform,
    normal = get_normal,
    exponential = get_exponential,
    Laplace = get_laplace,
    custom = get_custom
)
