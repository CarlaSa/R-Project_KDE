# Some probability density function factories.

#' Get a Cauchy distribution function.
#'
#' @param x0 A double vector of length 1. The location parameter.
#' @param gamma A double vector of length 1. The scale parameter. It should be greater than 0
#' @return A vectorised function. A Cauchy distribution function.
#' @export
get_cauchy <- function(x0 = 0, gamma = 0.5) {
    stopifnot("x0 should be a double vector of length 1" = is.double(x0) & length(x0) == 1)
    stopifnot("gamma should be a double vector of length 1" = is.double(gamma) & length(gamma) == 1 & gamma > 0)
    stopifnot("parameters should not be NAs" = !(is.na(x0) | is.na(gamma)))
    function(x)
        1/(pi * gamma * (1 + ((x - x0) / gamma)^2))
}

#' Get a uniform distribution function.
#'
#' @param a A double vector of length 1. A location parameter.
#' @param b A double vector of length 1. A location parameter. b should be bigger than a
#' @return A vectorised function. A uniform distribution function.
#' @export
get_uniform <- function(a = -1, b = 1) {
    stopifnot("a should be a double vector of length 1" = is.double(a) & length(a) == 1)
    stopifnot("b should be a double vector of length 1" = is.double(b) & length(b) == 1)
    stopifnot("b should be bigger than a" = a < b)
    stopifnot("parameters should not be NAs" = !(is.na(a) | is.na(b)))
    function(x)
        abs(1/(b-a)) * (a <= x & x <= b)
}

#' Get an exponential distribution function.
#'
#' @param lambda A double vector of length 1. It should be bigger than 0
#' The rate (inverse scale) parameter.
#' @return A vectorised function. An exponential distribution function.
#' @export
get_exponential <- function(lambda = 1) {
    stopifnot("lambda should be a double vector of length 1 bigger than 0" = is.double(lambda) & length(lambda) == 1 & lambda > 0)
    stopifnot("parameters should not be NAs" = !(is.na(lambda)))
    function(x){
        temp <- x>= 0
        temp[temp] <- lambda * exp(-lambda * x[temp])
        return(temp)
    }
}

#' Get a normal distribution function.
#'
#' @param mu A double vector of length 1. The mean.
#' @param sigma A double vector of length 1. The standard deviation. Should not be 0
#' @return A vectorised function. A normal distribution function.
#' @export
get_normal <- function(mu = 0, sigma = 1) {
    stopifnot("mu should be a double vector of length 1" = is.double(mu) & length(mu) == 1)
    stopifnot("sigma should be a double vector of length 1" = is.double(sigma) & length(sigma) == 1)
    stopifnot("sigma should not be zero" = sigma != 0)
    stopifnot("parameters should not be NAs" = !(is.na(mu) | is.na(sigma)))
    
    function(x)
        1/(sigma * sqrt(2 * pi)) * exp(-1/2 * ((x - mu)/sigma)^2)
}

#' Get a Laplace distribution function.
#'
#' @param mu A double vector of length 1. The mean.
#' @param b A double vector of length 1. The scale parameter. Should be greater than 0
#' @return A vectorised function. A Laplace distribution function.
#' @export
get_laplace <- function(mu = 0, b = 1) {
    stopifnot("mu should be a double vector of length 1" = is.double(mu) & length(mu) == 1)
    stopifnot("b should be a double vector of length 1" = is.double(b) & length(b) == 1)
    stopifnot("parameters should not be NAs" = !(is.na(mu) | is.na(b)))
    stopifnot("b should be greater than 0" = b > 0)
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
    stopifnot("str should be a character vector of lenght " = is.character(str) & length(str) == 1 & !is.na(str))
    function(x) eval(rlang::parse_expr(str))
}

#' Some preset probability density functions.
#' @usage Options: \code{pdfs$Cauchy}, \code{pdfs$Uniform}, \code{pdfs$Expo}, \code{pdfs$Mix2Gauss}
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
#' @details Users can use these function factories to set their own parameters.\cr
#' Options and defaults are as follows:  \cr
#' \code{pdf_factories$Cauchy(x0 = 0, gamma = 0.5)} \cr
#' \code{pdf_factories$Uniform(a = -1, b = 1)} \cr
#' \code{pdf_factories$Expo(lambda = 1)} \cr
#' \code{pdf_factories$Laplace(mu = 0, b = 1)} \cr
#' \code{pdf_factories$Custom(str = 'abs(x)/4 * (abs(x) <= 2)')} 
#' @export
pdf_factories <- list(
    Cauchy = get_cauchy,
    Uniform = get_uniform,
    Normal = get_normal,
    Expo = get_exponential,
    Laplace = get_laplace,
    Custom = get_custom
)
