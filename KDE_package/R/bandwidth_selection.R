#' Bandwidth selection using optimise.
#'
#' Find the optimal value for the bandwidth h.
#'
#' @param criterion_method A string. Keyword of the bandwidth selection algorithm to use. Implemented are: 
#' "CV" the Cross Validation method
#' "GL" the Goldenshluger-Lepski method
#' "PCO" the Penalised Comparison to Overfitting method
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param lower A double vector of length 1. The lowest bandwidth to test.
#' @param upper A double vector of length 1. The greatest bandwidth to test.
#' @param ... Optional arguments for the criterion function.
#' @param set_up_cluster A logical vector of length 1. Whether parallelisation should be activated if `setup_cluster` has not been run yet.
#' @return A double vector of length 1. The optimal bandwidth.
#' @export
bandwidth_selection <- function(criterion_method,
                                Kernel,
                                data,
                                lower = 1e-3,
                                upper = 1e0,
                                ...,
                                set_up_cluster = TRUE) {
  bws_methods <- bandwidth_selection_criteria()
  stopifnot("criterion_method must be `GL`,`PCO` or `CV`" = criterion_method %in% names(bws_methods))
  stopifnot("Kernel not valid" = is_Kernel(Kernel))
  if (set_up_cluster && !exists('cluster')) {
    n_cores <- setup_cluster()
    message(
      stringr::str_glue(
        'A cluster of {n_cores} cores has been set up and will be used in the future. See `?KDE::setup_cluster` for details.'
      )
    )
  }
  criterion_getter <- bws_methods[[criterion_method]]
  if ('lower' %in% formalArgs(criterion_getter)) {
    if ('upper' %in% formalArgs(criterion_getter)) {
      criterion <-
        criterion_getter(Kernel, data, lower = lower, upper = upper, ...)
    }
    else {
      criterion <- criterion_getter(Kernel, data, lower = lower, ...)
    }
  }
  else {
    criterion <- criterion_getter(Kernel, data, ...)
  }
  optimise(criterion, lower = lower, upper = upper)$minimum
}

#' Getters for the bandwidth selection criteria.
#'
#' @return A list of functions. The list of bandwidth selection criteria provided by the package.
#' @export
bandwidth_selection_criteria <- function() {
  list(CV = get_criterion_CV,
       GL = get_criterion_GL,
       PCO = get_criterion_PCO)
}
