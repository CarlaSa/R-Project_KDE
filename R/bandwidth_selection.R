#' Bandwidth selection using optimise.
#'
#' Find the optimal value for the bandwidth h.
#'
#' @param criterion A function. The bandwidth selection method. Must return a function accepting exactly one numeric parameter h (the bandwidth) and returning a number. This is the optimisation criterion, smaller values are considered better.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param lower A double vector of length 1. The lowest bandwidth to test.
#' @param upper A double vector of length 1. The greatest bandwidth to test.
#' @param ... Optional arguments for the criterion function.
#' @return A double vector of length 1. The optimal bandwidth.
#' @export
bandwidth_selection <- function(
                    criterion,
                    Kernel,
                    data,
                    lower = 1e-10,
                    upper = 1e1,
                    ...
                    ) {
    optimise(criterion(Kernel, data, lower, ...), lower = lower, upper = upper)$minimum
}
