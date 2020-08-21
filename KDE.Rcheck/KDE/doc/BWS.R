## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4
)

## ---- include=FALSE-----------------------------------------------------------
devtools::load_all(".")
library(ggplot2)
library(utils)

## ----setup--------------------------------------------------------------------
library(KDE)

## -----------------------------------------------------------------------------
pdf <- pdfs$Cauchy
data <- rejection_sample(1000, pdf)

## ---- echo=FALSE--------------------------------------------------------------
ggplot(data.frame(x=c(-3, 3)), aes(x=x)) + 
    geom_path(aes(colour="red"), stat="function", fun=kernels$gaussian)+
    geom_path(aes(colour="blue"), stat="function", fun=kernels$rectangular) +
    geom_path(aes(colour="green"), stat="function", fun=kernels$triangular) +
    geom_path(aes(colour="yellow"), stat="function", fun=kernels$epanechnikov) +
    geom_path(aes(colour="orange"), stat="function", fun=kernels$biweight) +
    geom_path(aes(colour="black"), stat="function", fun=kernels$silverman) +
    scale_colour_identity("Function", guide="legend", 
                          labels = c("Gaussian", "Rectangular", "Triangular", "Epanechnikov", "Biweight", "Silverman"),
                          breaks = c("red", "blue", "green", "yellow", "orange", "black")) 

## -----------------------------------------------------------------------------
kernel <- kernels$gaussian
system.time(
  h_opt <- bandwidth_selection('PCO', kernel, data, maxEval = 1e3)
  )
h_opt

## -----------------------------------------------------------------------------
KDE <- get_kde(h_opt, kernel, data)

## ---- echo=FALSE--------------------------------------------------------------
ggplot(data.frame(x=c(-3, 3)), aes(x=x)) + 
    geom_path(aes(colour="red"), stat="function", fun=KDE)+
    geom_path(aes(colour="black"), stat="function", fun=pdf) +
    scale_colour_identity("Function", guide="legend", 
                          labels = c("KDE", "True pdf"),
                          breaks = c("red", "black")) 

## ----eval=FALSE---------------------------------------------------------------
#  setup_cluster(use_parallel = TRUE)

## ----eval=FALSE---------------------------------------------------------------
#  setup_cluster(use_parallel = FALSE)

