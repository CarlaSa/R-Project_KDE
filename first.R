source('R/KDE.R', chdir=T)
source('R/rejection_sample.R', chdir=T)

n_obs <- 1000
f_real <- kernels$gaussian
data <- rejection_sample(n_obs, f_real)

lower <- min(data)
upper <- max(data)

x_plot <- seq(lower, upper, length.out = 100)
h <- 1

KDE_gaussian <- get_kde(h, kernels$gaussian, data)
KDE_epanechnikov <- get_kde(h, kernels$epanechnikov, data)

kde_gaussian_plot <- KDE_gaussian(x_plot)
kde_epanechnikov_plot <- KDE_epanechnikov(x_plot)

plot(x_plot, f_real(x_plot), type = 'l')
lines(x_plot, kde_gaussian_plot, col = 2)
lines(x_plot, kde_epanechnikov_plot, col = 3)
legend('topleft',
       c('true function',
         'Gaussian KDE',
         'Epanechnikov KDE'),
       col = 1:3,
       lwd = 1)
