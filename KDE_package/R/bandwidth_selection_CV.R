# Bandwidth selection Cross Validation

#' Criterion Cross Validation
#' We want to choose h in such a way, that criterion is minimized
#' 
#' @param h A double vector of length 1. The bandwidth.
#' @param Kernel A real function. The kernel.
#' @param data A double vector of the sample data to use.
#' @return a single value, the criterion Cross Validation is minimizing
#' @export
#' 
criterion_CV <- function(h, Kernel, data){
  n <- length(data)
  f <- get_kde(h = h, Kernel = Kernel, data = data)
  
  term1 <- L2norm_squared(f)
  
  temp <- 0
  for (i in 1:n){
    for (j in 1: i){
      if (i != j) temp = temp + Kernel( (Z[i] - Z[j]) / h )
    }
  }
  
  term2 <- 2/(n * (n-1) * h)  * sum(temp)
  
  return(term1- term2)
}


