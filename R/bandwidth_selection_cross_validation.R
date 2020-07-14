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
  0.75 * (x <1 & x > -1) * (1 - x^2)
}

criterion <-function(h, Kernel, Z ,n, l, u){
  # h is the bandwidth
  # f is the estimator of our true function
  # K is the Kernel function
  # n is
  # lower and upper are the boundaries of the integrate term
  # Z is the data
  f_hut <- get_kde(n = n,h = h, Kernel = Kernel ,data =  Z)
  f_hut_squ <- function(x) f_hut(x)^2 
  term1 <- integrate(Vectorize(f_hut_squ), lower = -10, upper = 10, stop.on.error = F)$value
  `%tempfun%` <- function(i,j){
      is_equal <- i == j
      (1- is_equal) * Kernel( (Z[i] - Z[j]) / h )
  }
  temp <- outer(
    X = 1: n,
    Y = 1: n,
    FUN = `%tempfun%`
  )
  term2 <- 2 * 1/(n * (n-1) * h)  * sum(temp)
  return(term1 - term2)
}

#bandwidth selection
# bws_cross_validation <- function(J, bound, ...){
#   #find lokal minimum of J in variable h within bound
#   h = optimize(J, interval = bound, ...)
#   return(h)
# }
bws_cross_validation <- function(J, len, ...){
  #values = seq(minval, maxval, length.out = len +2)[-1]
  values = 1/ (1:len)
  hmin = 0
  Jmin = Inf
  for (h in values){
    if (J(h, ...) < Jmin){
      hmin <- h
    }
  }
  return(h)
}

get_kde <- function(n, h, Kernel, data){
  f <-function(x){
    1/(n*h) * sum(
      Kernel((data - x)/h)
    )
  }
  return(f)
}

h <- bws_cross_validation(J = criterion, len = 40, Kernel = e_kernel, n = nobs, l = -1e5, u = 1e5, Z = Z )
kde <- get_kde(Kernel = e_kernel, n = nobs, h = h, data = Z )
plot(true_function)
lines(Vectorize(kde)(ab), col = "red")
lines(true_function)
