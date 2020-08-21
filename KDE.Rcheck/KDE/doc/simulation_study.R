## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4
)

## ---- include=FALSE-----------------------------------------------------------
devtools::load_all(".")
library(ggplot2)
library(tidyverse)

## ----setup--------------------------------------------------------------------
library(KDE)

## -----------------------------------------------------------------------------
pdf <- pdfs$Expo
data <- rejection_sample(1000, pdf)
kernel <- kernels$gaussian

## ----eval=FALSE---------------------------------------------------------------
#  system.time(
#    h_opt_GL <- bandwidth_selection('GL', kernel, data, maxEval = 100, n_bandwidths=3)
#    )
#  #> Warning in criterion_getter(Kernel, data, maxEval, lower = lower, upper =
#  #> upper, : You have chosen the lowest bandwidth smaller than 0.1. Taking value
#  #> `lower = 0.1`.
#  #>    user  system elapsed
#  #>  53.356   1.418  55.186
#  h_opt_GL
#  #> [1] 0.1025965

## ----eval = FALSE-------------------------------------------------------------
#  system.time(
#    h_opt_PCO <- bandwidth_selection('PCO', kernel, data, maxEval = 1e3)
#    )
#  #>    user  system elapsed
#  #>   2.853   0.123   2.996
#  h_opt_PCO
#  #> [1] 0.1468662

## ----eval=FALSE---------------------------------------------------------------
#  system.time(
#    h_opt_CV <- bandwidth_selection('CV', kernel, data, maxEval = 1e3)
#    )
#  #>    user  system elapsed
#  #>  41.994   0.243  42.762
#  h_opt_CV
#  #> [1] 0.03334418

## ----include=FALSE------------------------------------------------------------
h_opt_GL <- 0.1025965
h_opt_PCO <- 0.05380332
h_opt_CV <- 0.03334418


## -----------------------------------------------------------------------------
KDE_GL <- get_kde(h_opt_GL, kernel, data)
KDE_PCO <- get_kde(h_opt_PCO, kernel, data)
KDE_CV <- get_kde(h_opt_CV, kernel, data)

## ----echo=FALSE---------------------------------------------------------------
ggplot(data.frame(x=c(-3, 3)), aes(x=x)) + 
    geom_path(aes(colour="red"), stat="function", fun=KDE_GL)+
    geom_path(aes(colour="blue"), stat="function", fun=KDE_PCO) +
    geom_path(aes(colour="green"), stat="function", fun=KDE_CV) +
    geom_path(aes(colour="black"), stat="function", fun=pdf) +
    scale_colour_identity("Function", guide="legend", 
                          labels = c("KDE_GL", "KDE_PCO", "KDE_CV", "True pdf"),
                          breaks = c("red", "blue", "green", "black")) 

## ----eval=FALSE---------------------------------------------------------------
#  compare <- function(x_points=seq(-1, 1, len=600),
#                      real_functions=list(cauchy = pdfs$Cauchy),
#                      n=1000,
#                      try_kernels=list(gaussian=kernels$gaussian),
#                      methods=list(CV="CV"),
#                      reps=5){
#    if(!is.list(try_kernels)| is.null(names(try_kernels))) stop("try_kernels must be named list")
#    if(!is.list(real_functions)| is.null(names(real_functions))) stop("real_functions must be named list")
#    if(!is.list(methods)| is.null(names(methods))) stop("methods must be named list")
#    d <- length(real_functions)*length(try_kernels)*length(methods)
#    res <- array(NA, dim=c(length(x_points), reps, d))
#    cnt <- 1
#    # iterate through all vectors of options
#    for (f in real_functions){
#      for (k in try_kernels){
#        for (m in methods){
#          cat("Working on combination: ", cnt, "of", d, "\n", "Method:", m, "\n")
#          res[,, cnt] <- replicate(reps, {
#            data <- rejection_sample(n,f)
#            h <- bandwidth_selection(m, k, data, maxEval=1e3)
#            KDE <- get_kde(h, k, data)
#            KDE(x_points)
#          })
#          cnt <- cnt + 1
#        }
#      }
#    }
#    method_str <- paste(names(methods), collapse="_")
#    func_str <- paste(names(real_functions), collapse="_")
#    kernel_str <- paste(names(try_kernels), collapse="_")
#    saveRDS(res, file=paste(kernel_str, method_str, func_str, reps, "res.RDS", sep="_"))
#    res
#  }

## ----eval=FALSE, include=FALSE------------------------------------------------
#  # helper function for plotting
#  plot_with_confidence_band <- function(x, Y, col) {
#    rgb <- col2rgb(col)/255
#    col_alpha <- rgb(rgb[1], rgb[2], rgb[3], 0.2)
#    v <- apply(Y, 1, function(x) c(mean(x), sd(x)))
#    lines(x, v[1, ], lwd=2, col=col)
#    polygon(
#      c(x, rev(x)),
#      c(v[1, ]+v[2, ], rev(v[1, ]-v[2, ])),
#      col = col_alpha,
#      border = NA)
#    lines(x, v[1, ]+v[2, ], lwd=1, col=col)
#    lines(x, v[1, ]-v[2, ], lwd=1, col=col)
#  }

## ----eval=FALSE, include=FALSE------------------------------------------------
#  plot_comparison <- function(f=list(cauchy = pdfs$Cauchy),
#                              main=NA,
#                              legend=NULL,
#                              show_diff=TRUE,
#                              reps=50,
#                              ...) {
#    x_new <- seq(-1, 1, len=600)
#    res <- compare(x_new, f, reps=reps, ...)
#    m <- dim(res)[3]
#    del <- max(apply(res, c(1, 3), sd))
#    f_new <- f(x_new)
#    # plot results
#    par(mar = c(0,0,1,0), ann=FALSE, xaxt="n", yaxt="n")
#    plot(x_new, f_new, type="l", lwd=2, col=1, ylim = range(f_new) + del*c(-1, +1))
#    grid()
#    for (i in 1:m) plot_with_confidence_band(x_new, res[,,i], col=i+1)
#    title(main = main)
#    if (!is.null(legend)) legend("topright", legend=legend, lwd=2, col=2:(m+1))
#    # create second plot (difference between true f and estimation)
#    if (show_diff) {
#      diff <- res - f_new
#      plot(c(-1,1), c(0,0), type="l", lwd=2, col=1, ylim = c(-del, del))
#      grid()
#      for (i in 1:m) plot_with_confidence_band(x_new, diff[,,i], col=i+1)
#    }
#  }

## ----eval=FALSE---------------------------------------------------------------
#  plot_comparison(methods=list(CV="CV", PCO="PCO"),
#                  main = "n=1000, Cauchy distribution, Gaussian Kernel",
#                  legend = c("CV", "PCO"))

## ----eval=FALSE---------------------------------------------------------------
#  compare_ise <- function(real_functions = list(cauchy=pdfs$Cauchy),
#                          n=1000,
#                          try_kernels=list(gaussian=kernels$gaussian),
#                          methods=list(CV="CV"),
#                          reps=3){
#    x_points <- seq(-1, 1, length.out=600)
#    time <- system.time(
#      res <- compare(x_points,
#                     real_functions = real_functions,
#                     n = n,
#                     try_kernels = try_kernels,
#                     methods = methods,
#                     reps = reps)
#    )
#    print(time)
#    f_true <- sapply(real_functions, function(f) f(x_points))
#    f_true <- f_true[,rep(seq_along(real_functions), each=length(try_kernels)*length(methods))]
#    diff <- array(NA, dim=dim(res))
#    for (j in 1:dim(res)[3]) diff[,,j] <- res[,,j] - f_true[,j]
#    ise <- apply(diff^2, c(2, 3), mean)
#    opts <- as_tibble(expand.grid(method=names(methods), kernel=names(try_kernels), func= names(real_functions)))
#    return_obj <- add_column(opts[rep(1:nrow(opts), each=reps), ], ise=as.vector(ise))
#    method_str <- paste(names(methods), collapse="_")
#    func_str <- paste(names(real_functions), collapse="_")
#    kernel_str <- paste(names(try_kernels), collapse="_")
#    saveRDS(return_obj, file=paste(kernel_str, method_str, func_str, reps, "ISE.RDS", sep="_"))
#    return_obj
#  }

## ----eval=FALSE---------------------------------------------------------------
#  try_kernels <- list(epanechnikov=kernels$epanechnikov, gaussian=kernels$gaussian)
#  methods <- list(CV="CV", PCO="PCO")
#  real_functions <- list(cauchy=pdfs$Cauchy, normal = get_normal(0,1))
#  reps <- 100
#  data_ise <- compare_ise(real_functions, try_kernels=try_kernels, methods=methods, reps=reps)

## ----eval=FALSE, include=FALSE------------------------------------------------
#  #NOTE: REMOVE THIS SECTION ONCE FILES ARE GENERATED
#  data_ise2 <- readRDS("epanechnikov_CV_PCO_cauchy_normal_100_ISE.RDS")
#  data_ise3 <- readRDS("gaussian_CV_PCO_normal_100_ISE.RDS")
#  
#  data_ise2 %>%
#    group_by(func, kernel, method) %>%
#    summarise(mise = mean(ise), med_ise=median(ise), sd_ise = sd(ise), reps = n()) %>%
#    ungroup() ->
#    data_mise2
#  
#  data_ise3 %>%
#    group_by(func, kernel, method) %>%
#    summarise(mise = mean(ise), med_ise=median(ise), sd_ise = sd(ise), reps = n()) %>%
#    ungroup() ->
#    data_mise3
#  
#  saveRDS(data_mise2, file="epanechnikov_CV_PCO_cauchy_normal_100_MISE.RDS")
#  saveRDS(data_mise3, file="gaussian_CV_PCO_normal_100_MISE.RDS")

## ----eval=FALSE---------------------------------------------------------------
#  data_ise %>%
#    group_by(func, kernel, method) %>%
#    summarise(mise = mean(ise), med_ise=median(ise), sd_ise = sd(ise), reps = n()) %>%
#    ungroup() ->
#    data_mise
#  
#  kernel_str <- paste(names(try_kernels), collapse="_")
#  method_str <- paste(names(methods), collapse="_")
#  func_str <- paste(names(real_functions), collapse="_")
#  
#  saveRDS(data_mise, file=paste(kernel_str, method_str, func_str, reps, "MISE.RDS", sep="_"))

## ----eval=TRUE, include=FALSE-------------------------------------------------
# Here's where we add all our objects together to create data_mise_full:
mise1 <- readRDS("gaussian_CV_PCO_cauchy_100_MISE.RDS")
mise2 <- readRDS("epanechnikov_CV_PCO_cauchy_normal_100_MISE.RDS")
mise3 <- readRDS("gaussian_CV_PCO_normal_100_MISE.RDS")
data_mise <- bind_rows(mise1, mise2, mise3)
data_mise

## -----------------------------------------------------------------------------
data_mise

## -----------------------------------------------------------------------------
data_mise %>%
  select(method, kernel, mise) %>%
  pivot_wider(names_from=kernel, values_from=mise, values_fn=mean) -> kernels_mise
kernels_mise

## -----------------------------------------------------------------------------
data_mise %>%
  select(method, func, mise) %>%
  pivot_wider(names_from = func, values_from=mise, values_fn=mean) -> functions_mise
functions_mise

