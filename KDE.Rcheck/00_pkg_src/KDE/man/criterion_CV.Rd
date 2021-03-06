% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bandwidth_selection_CV.R
\name{criterion_CV}
\alias{criterion_CV}
\title{Criterion Cross Validation
We want to choose h in such a way, that criterion is minimized}
\usage{
criterion_CV(h, Kernel, data, maxEval)
}
\arguments{
\item{h}{A double vector of length 1. The bandwidth.}

\item{Kernel}{A real function. The kernel.}

\item{data}{A double vector of the sample data to use.}

\item{maxEval}{A double vector of length 1. The maximum number of function evaluations when integrating.}
}
\value{
a single value, the criterion Cross Validation is minimizing
}
\description{
Criterion Cross Validation
We want to choose h in such a way, that criterion is minimized
}
