source('kernels.R')
source('rejection_sample.R')
source('L2norm.R')

K_h <- function(h, t, Kernel) {
    Kernel(t / h) / h
}

f_hat <- function(h, data, v, n_obs, Kernel) {
    sum(K_h(h, data-v, Kernel)) / n_obs
}

B_hat <- function(h, m, data, n_obs, Kernel) {
    # comparison to overfitting
    L2norm_squared(sapplify(function(v) {
                                f_hat(h, data, v, n_obs, Kernel) -
                                    f_hat(m, data, v, n_obs, Kernel)
})) -
             # penalized
             L2norm_squared(sapplify(function(t) {
                                         K_h(h, t, Kernel) -
                                             K_h(m, t, Kernel)
})) / n_obs
}

V_hat <- function(h, x, n_obs, Kernel) {
    x * L2norm_squared(Kernel) / (n_obs * h)
}

Risk_hat <- function(h, m, data, x, n_obs, Kernel) {
    B_hat(h, m, data, n_obs, Kernel) +
        V_hat(h, x, n_obs, Kernel)
}

sapplify <- function(f) {
    function(a) {
        sapply(a, f)
    }
}

criterion <- function(m, data = rejection_sample(n_obs, Kernel), x, n_obs, Kernel) {
    force(m)
    force(x)
    force(n_obs)
    force(Kernel)
    force(data)
    function(bandwidths) {
        sapply(bandwidths, function(h) Risk_hat(h, m, data, x, n_obs, Kernel))
    }
}

bws_PCO <- function(
                    Kernel = kernels$gaussian,
                    n_obs = 100,
                    bandwidths = 1 / 1:100,
                    x = 1,
                    data = rejection_sample(n_obs, Kernel)
                    ) {
    risks <- criterion(min(bandwidths), data, x, n_obs, Kernel)(bandwidths)
    bandwidths[which.min(risks)]
}
