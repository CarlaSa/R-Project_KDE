#' Bandwidth selection using optimise.
#'
#' Find the optimal value for the bandwidth h.
#'
#' @param criterion_name Keyword of the bandwidth selection algorithm to use. Implimented are: 
#' "CV" the Cross Validation method
#' "GL" the Goldenshluger-Lepski method
#' "PCO" the PCO method
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param lower A double vector of length 1. The lowest bandwidth to test.
#' @param upper A double vector of length 1. The greatest bandwidth to test.
#' @param ... Optional arguments for the criterion function.
#' @return A double vector of length 1. The optimal bandwidth.
#' @export
bandwidth_selection <- function(
  criterion_name,
  Kernel,
  data,
  lower = 1e-10,
  upper = 1e1,
  ...
) {
  criterion <- bandwidth_selection_criteria()[[criterion_name]](Kernel, data, lower, ...)
  optimise(criterion, lower = lower, upper = upper)$minimum
}

#' Getters for the bandwidth selection criteria.
#' 
#' 
#'
#' @export
bandwidth_selection_criteria <- function(){
  list(
    CV = get_criterion_CV,
    GL = get_criterion_GL,
    PCO = get_criterion_PCO 
    )}
  
