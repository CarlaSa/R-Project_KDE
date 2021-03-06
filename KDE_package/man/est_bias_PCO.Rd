% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bandwidth_selection_PCO.R
\name{est_bias_PCO}
\alias{est_bias_PCO}
\title{Estimator for the Bias Term}
\usage{
est_bias_PCO(h, Kernel, data, maxEval, m)
}
\arguments{
\item{h}{A double vector of length 1. The bandwidth.}

\item{Kernel}{A real function. The kernel.}

\item{data}{A double vector of the sample data to use.}

\item{maxEval}{A double vector of length 1. The maximum number of function evaluations when integrating.}

\item{m}{A double vector of length 1. The smallest bandwidth.}
}
\value{
A double vector of length 1.
}
\description{
B_hat
}
