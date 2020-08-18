#' Bandwidth selection using optimise.
#'
#' Find the optimal value for the bandwidth h.
#'
#' @param criterion_getter A function. The bandwidth selection method. Must return a function accepting exactly one numeric parameter h (the bandwidth) and returning a number. This is the optimisation criterion, smaller values are considered better.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param lower A double vector of length 1. The lowest bandwidth to test.
#' @param upper A double vector of length 1. The greatest bandwidth to test.
#' @param ... Optional arguments for the criterion function.
#' @return A double vector of length 1. The optimal bandwidth.
#' @export
bandwidth_selection <- function(
                    criterion_getter,
                    Kernel,
                    data,
                    lower = 1e-10,
                    upper = 1e1,
                    ...
                    ) {
    optimise(criterion_getter(Kernel, data, lower, ...), lower = lower, upper = upper)$minimum
}

source('bandwidth_selection_PCO.R')
#' Getters for the bandwidth selection criteria.
#' 
#' 
#'
#' @export
bandwidth_selection_criteria <- list(
                                     # CV = get_criterion_CV,
                                     # GL = get_criterion_GL,
                                     PCO = get_criterion_PCO
)
