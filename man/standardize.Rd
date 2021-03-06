% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{standardize}
\alias{standardize}
\title{Standardize Data}
\usage{
standardize(x, center = TRUE, normalize = FALSE)
}
\arguments{
\item{x}{data to be standardized}

\item{center}{Should \code{x} be centered. Default is \code{TRUE}}

\item{normalize}{Should \code{x} be scaled to have unit variance. Default is
\code{FALSE}}
}
\value{
list of length 3: \describe{ \item{x}{centered and possibly
  normalized \code{x} matrix} \item{bx}{numeric vector of column means of
  \code{x} matrix} \item{sx}{standard deviations (using a divisor of
  \code{n} observations) of columns of \code{x} matrix} }
}
\description{
Function that standardizes the data before running the fitting
  algorithm. This is used in the \code{\link{sail}} function
}
