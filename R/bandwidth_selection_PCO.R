source('kernels.R')

K_h <- function(h, t, test) {
    test$K(t / h) / h
}

f_hat <- function(h, X, x, test) {
    sum(K_h(h, X-x, test)) / test$n
}

B_hat <- function(h, m, X, test) {
    # comparison to overfitting
    L2norm_squared(sapplify(function(x) {
                                f_hat(h, X, x, test) -
                                    f_hat(m, X, x, test)
})) -
             # penalized
             L2norm_squared(sapplify(function(t) {
                                         K_h(h, t, test) -
                                             K_h(m, t, test)
})) / test$n
}

V_hat <- function(h, test) {
    test$x * L2norm_squared(test$K) / (test$n * h)
}

Risk_hat <- function(h, m, X, test) {
    B_hat(h, m, X, test) +
        V_hat(h, test)
}

sapplify <- function(f) {
    function(x) {
        sapply(x, f)
    }
}

L2norm_squared <- function(f) {
    integrate(function(u) f(u)^2, lower = -Inf, upper = Inf)$value
}

L2norm <- function(f) {
    sqrt(L2norm_squared(f))
}

h_hat <- function(
                  K = kernels$gaussian,
                  n = 100, # sample size
                  Hn = 100, # number of bandwidths
                  possible_bandwidths = 1 / 1:test$Hn,
                  distrib = function(n) rnorm(n, 0, 1)
                  ) {
    m <- min(possible_bandwidths)
    X <- distrib(n)
}

