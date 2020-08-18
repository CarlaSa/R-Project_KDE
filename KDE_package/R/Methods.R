# Functions that are used by all Methods

# Tools ####

#' Vectorise a single-parameter function using sapply.
#'
#' @param f A single-parameter function.
#' @return A vectorised single-parameter function.
sapplify <- function(f) {
  function(a) {
    sapply(a, f)
  }
}

# L2 Norm ####

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
  if(is_Kernel(f))
    return(attr(f, 'L2norm_squared'))
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
  if(is_Kernel(f))
    return(attr(f, 'L2norm'))
  sqrt(L2norm_squared(f))
}

# Kernels ####
#' Constructor for a Kernel object.
#'
#' @param func A function. The kernel function. Must be vectorised.
#' @return A Kernel object.
#' @export
Kernel <- function(func) {
  K <- func
  l2_sq <- L2norm_squared(K)
  attr(K, 'L2norm_squared') <- l2_sq
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
#' @return A logical.
#' @export
is_Kernel <- function(object, check_values = FALSE, tolerance_rel = 1e-12) {
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

# KDE ####

#' Scaled kernel.
#'
#' @param x A double vector. The argument.
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @return A double vector.
#' @export
scaled_kernel <- function(x, h, Kernel) {
  Kernel(x / h) / h
}

#' Kernel density estimator.
#'
#' @param x A double vector of length 1. The argument.
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @return A double vector of length 1.
#' @export
kde <- function(x, h, Kernel, data) {
  sum(scaled_kernel(data-x, h, Kernel)) / length(data)
}

#' Get a fix KDE.
#'
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @return A double vector of length 1.
#' @export
get_kde <- function(h, Kernel, data) {
  function(x) {
    sapply(x, kde, h, Kernel, data)
  }
}

# Convolution ####
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

