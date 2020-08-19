# Bandwidth selection Cross Validation

#' Criterion Cross Validation
#' We want to choose h in such a way, that criterion is minimized
#' 
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @return a single value, the criterion Cross Validation is minimizing
#' 
criterion_CV <- function(h, Kernel, data, maxEval){
  n <- length(data)
  f <- get_kde(h = h, Kernel = Kernel, data = data)
  term1 <- L2norm_squared(f, maxEval)
  
  temp <- 0
  for (i in 1:n){
    for (j in 1:n){
      if (i != j) temp = temp + Kernel( (data[i] - data[j]) / h )
    }
  }
  
  
  term2 <- 2/(n * (n-1) * h)  * sum(temp)
  return(term1- term2)
}

#' Optimisation criterion for bandwidth selection using the Cross Validation method.
#'
#' Returns the Cross Validation Criterion for fixed Kernel and data only dependend on h
#' Minimise it to find the optimal value for h.
#'
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @param maxEval A double vector of length 1. The maximum number of function evaluations when integrating.
#' @return A vectorised single-parameter function. The Cross Validation bandwidth selection
#' optimisation criterion.
get_criterion_CV <- function(Kernel, data, maxEval = 1e6){
    force(Kernel)
    force(data)
    force(maxEval)
    function(hvals) {
        sapply(hvals, criterion_CV, Kernel, data, maxEval)
    }
}

