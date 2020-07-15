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

K_h <- function(h, X, x, test) {
    test$K((X-x) / h) / h
}

f_hat <- function(h, X, x, test) {
    sum(K_h(h, X, X-x, test)) / test$n
}

B_hat <- function(h, m, X, test) {
    # comparison to overfitting
    L2norm_squared(sapplify(function(x) {
                                f_hat(h, X, x, test) -
                                    f_hat(m, X, x, test)
})) -
             # penalized
             L2norm_squared(sapplify(function(x) {
                                         K_h(h, X, x, test) -
                                             K_h(m, X, x, test)
})) / test$n
}

V_hat <- function(h, x, test) {
    x * L2norm_squared(test$K) / (test$n * h)
}

Risk_hat <- function(h, m, X = X, x = x, test = test) {
    B_hat(h, m, X, test) +
        V_hat(h, x, test)
}

sapplify <- function(f) {
    function(x) {
        sapply(x, f)
    }
}

L2norm_squared <- function(f) {
    integrate(function(u) f(u)^2, lower = -Inf, upper = Inf)
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

