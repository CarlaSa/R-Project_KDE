source('R/bandwidth_selection_PCO.R', chdir=T)

## Gaussian

Kernel <- kernels$gaussian
n_obs <- 1000
data <- rejection_sample(n_obs, Kernel)
h_opt <- bws_PCO(Kernel, data)
KDE <- get_kde(h_opt, Kernel, data)

x_vals <- seq(from = -3, to = 3, length.out = 1e5)
kde_vals <- KDE(x_vals)

plot(x_vals, kde_vals, col = 2, lwd = 2, type = 'l')
lines(x_vals, Kernel(x_vals))
legend('topleft',
    c('true function', 'KDE'),
    lwd = 1:2,
    col = 1:2