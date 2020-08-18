# simulation of the model
nobs = 1000
alpha = 3
beta = 3
Z1 = rexp(rate = 1, n = nobs) - rexp(rate = 1, n = nobs)
Z2 = rbeta(n = nobs,shape1 = alpha, shape2 = beta)
Z3 = rnorm(nobs)

#domain and true function
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


#epanechnikov kernel
e_kernel <- function(x){
  0.75 * (x <1 & x > -1) * (1 - x^2)
}

criterion <-function(h, Kernel, Z , n, l, u){
  #' We want to choose h in such a way, that criterion is minimized
  #' 
  #' h is the bandwidth
  #' f is the estimator of our true function
  #' K is the Kernel function
  #' n is
  #' lower and upper are the boundaries of the integrate term
  #' Z is the data
  
  f_hut <- get_kde(n = n,h = h, Kernel = Kernel, data =  Z)
  f_hut_squ <- function(x) f_hut(x)^2 
  
  get_integral <- function(f){
    
    #get boundaries
    # lower boundary
    dots = - 2^(4:-10)
     low = -Inf
    # for (d in dots){
    #   if(f(d) == 0){
    #     low = d
    #   }
    #   else{
    #     break
    #   }
    # }
     

    #upper boundary
     upp = Inf
    # dots = 2^(4:-10)
    # for (d in dots){
    #   if(f(d) == 0){
    #     upp = d
    #   }
    #   else{
    #     break
    #   }
    # }
    
    #print(c(low, upp))
    
    for (subd in 100 * 1:8){
      t = try(integrate(Vectorize(f), lower = low, upper = upp, subdivisions =  subd)$value, silent = T)
      if (class(t) != "try-error"){
        break
      }
    }
    if (class(t) == "try-error"){
      print("subdivisions zu klein")
      print(t)
      t = Inf
    }
    
    if (t == 0){
      print("integral 0 :(")
    }
    return(t)
  }

  term1 <- get_integral(f_hut_squ)
  
  `%tempfun%` <- function(i,j){
      is_equal <- i == j
      (1- is_equal) * Kernel( (Z[i] - Z[j]) / h )
  }
  temp <- outer(
    X = 1: n,
    Y = 1: n,
    FUN = `%tempfun%`
  )
  term2 <- 1/(n * (n-1) * h)  * sum(temp)
  print(c("term1 ", term1, "term2 ", term2))
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
    time <- Sys.time()
    J_temp = J(h, ...)
    if (J_temp < Jmin){
      hmin <- h
    }
    print(str_c("h is ", h, " time for calculating J: ", Sys.time()- time," J is ",  J_temp) )
  }
  print(c("hmin", hmin))
  return(h)
}

get_kde <- function(n, h, Kernel, data){
  #'
  #'
  f <-function(x){
    1/(n*h) * sum(
      Kernel((data - x)/h)
    )
  }
  return(f)
}

h <- bws_cross_validation(J = criterion, len = 40, Kernel = e_kernel, n = nobs, l = -1e5, u = 1e5, Z = Z1 )
kde <- get_kde(Kernel = e_kernel, n = nobs, h = h, data = Z1)
plot(true_function1, type = "l")
lines(Vectorize(kde)(ab1), col = "red")
lines(density(Z1))
