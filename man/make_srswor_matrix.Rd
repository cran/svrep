% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quadratic_forms.R
\name{make_srswor_matrix}
\alias{make_srswor_matrix}
\title{Create a quadratic form's matrix to represent the basic variance estimator
for a total under simple random sampling without replacement}
\usage{
make_srswor_matrix(n, f = 0)
}
\arguments{
\item{n}{Sample size}

\item{f}{A single number between \code{0} and \code{1},
representing the sampling fraction. Default value is \code{0}.}
}
\value{
A symmetric matrix of dimension \code{n}
}
\description{
The usual variance estimator for simple random sampling without replacement
can be represented as a quadratic form.
This function determines the matrix of the quadratic form.
}
\details{
The basic variance estimator of a total for simple random sampling without replacement is as follows:
\deqn{
\hat{v}(\hat{Y}) = (1 - f)\frac{n}{n - 1} \sum_{i=1}^{n} (y_i - \bar{y})^2
}
where \eqn{f} is the sampling fraction \eqn{\frac{n}{N}}. \cr \cr
If \eqn{f=0}, then the matrix of the quadratic form has all non-diagonal elements equal to \eqn{-(n-1)^{-1}},
and all diagonal elements equal to \eqn{1}. If \eqn{f > 0}, then each element
is multiplied by \eqn{(1-f)}. \cr \cr
If \eqn{n=1}, then this function returns a \eqn{1 \times 1} matrix whose sole element equals \eqn{0}
(essentially treating the sole sampled unit as a selection made with probability \eqn{1}).
}
\keyword{internal}
