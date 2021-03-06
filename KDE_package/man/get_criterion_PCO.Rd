% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bandwidth_selection_PCO.R
\name{get_criterion_PCO}
\alias{get_criterion_PCO}
\title{Optimisation criterion for bandwidth selection using PCO.}
\usage{
get_criterion_PCO(Kernel, data, maxEval, lower, v = 1)
}
\arguments{
\item{Kernel}{A real function. The kernel.}

\item{data}{A double vector of the sample data to use.}

\item{maxEval}{A double vector of length 1. The maximum number of function evaluations when integrating.}

\item{lower}{A double vector of length 1. The lowest bandwidth, used for comparison to overfitting.}

\item{v}{A double vector of length 1. A calibration constant.}
}
\value{
A vectorised single-parameter function. The PCO bandwidth selection
optimisation criterion.
}
\description{
The Estimator for the Risk with all parameters fixed but the bandwidth h.
Minimise it to find the optimal value for h.
}
