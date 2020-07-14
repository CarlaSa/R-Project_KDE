

get_kde <- function(n, h, Kernel, data){
  f <-function(x){
    1/(n*h) * sum(
      Kernel((data - x)/h)
    )
  }
  return(f)
}

# generate data with true functions##############

nobs = 1000
alpha = 3
beta = 3
Z1 = rexp(rate = 1, n = nobs) - rexp(rate = 1, n = nobs)
Z2 = rbeta(n = nobs,shape1 = alpha, shape2 = beta)
Z3 = rnorm(nobs)

a1 = min(Z1)
a2 = min(Z2)
a3 = min(Z3)
b1 = max(Z1)
b2 = max(Z2)
b3 = max(Z3)
K = 50
ab1 = seq(a1,b1,length.out = K)
true_function1 = exp(-abs(ab1))/2
ab2 = seq(a2,b2,length.out = K)
true_function2 = dbeta(x = ab2, shape1 = alpha, shape2 = beta)
ab3 = seq(a3,b3,length.out = K)
true_function3 = dnorm(x = ab3)

# get kernel #########
kernels <- c(
  gaussian = function(u) {
    1 / (sqrt(2*pi)) * exp(-u^2 / 2)
  },
  rectangular = function(u) {
    1/2 * (abs(u) <= 1)
  },
  triangular = function(u) {
    (1 - abs(u)) * (abs(u) <= 1)
  },
  epanechnikov = function(u) {
    3/4 * (1 - u^2) * (abs(u) <= 1)
  },
  biweight = function(u) {
    15/16 * (1 - u^2)^2 * (abs(u) <= 1)
  },
  silverman = function(u) {
    1/2 * exp(-abs(u)/sqrt(2)) *
      sin(abs(u)/sqrt(2) + pi/4)
  }
)
kernels$parabolic <- kernels$epanechnikov

# get bandwidth #########

bandwidths <- c(
  # cross validation
  h_cv <- NA, 
  # Goldenshluger-Lepski
  h_gl <- NA,
  h_pco <- NA
)
# plot #####


