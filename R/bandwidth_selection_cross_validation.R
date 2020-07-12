# simulation of the model

nobs = 1000
alpha = 3
beta = 3
Z = rexp(rate = 1, n = nobs) - rexp(rate = 1, n = nobs)

#domain and true function
a = min(Z)
b = max(Z)
K = 50
ab = seq(a,b,length.out = K)
true_function = exp(-abs(ab))/2

#epanechnikov kernel
e_kernel <- function(x){
  (x <1 & x > -1) * (1 - x^2)
}

criterion <-function(h, f, K, Z ,n, lower, upper){
  # h is the bandwidth
  # f is the estimator of our true function
  # K is the Kernel function
  # n is
  # lower and upper are the boundaries of the integrate term
  # Z is the data
 
  term1 <- integrate(f = function(x){f(x)^2}, lower = lower, upper = upper)
  temp <- outer(
    X = 1: n,
    Y = 1: n,
    FUN = function(i,j){
      is_equal <- i == y
      (1- is_equal) * K( (Z[i] - Z[j]) / h )
    }
  )
  term2 <- 2 * 1/(n * (n-1) * h)  * sum(temp)
  return(term1 + term2)
}

#bandwidth selection
bws_cross_validation <- function(J, bound, ...){
  #find lokal minimum of J in variable h within bound
  h = optimize(J, interval = bound, ...)
  return(h)
}

get_kde <- function(n, h, K, data){
  f <-function(x){
    temp <- (data -x) / h
    1/(n*h) * sum(sapply(temp, K))
  }
  return(f)
}

h <- bws(J = criterion, bound = c(-100,100), f = )
kde <- get_kde(1000, )

