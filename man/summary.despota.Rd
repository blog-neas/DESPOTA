% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{summary.despota}
\alias{summary.despota}
\title{summary.despota}
\usage{
\method{summary}{despota}(object, ...)
}
\arguments{
\item{object}{a despota object}

\item{...}{additional options passed to the summary function}
}
\value{
a membership vector of the despota model
}
\description{
summarize a despota class object
}
\examples{
data <- c(1,2,3,4,5, 7,9,10,11,12, 19,24,28,32,38, 54)
out <- despota(data, alpha = .05, seed = 31, listVal = TRUE, par = FALSE)
summary(out)
}
