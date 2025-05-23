% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quadratic_forms.R
\name{make_sd_matrix}
\alias{make_sd_matrix}
\title{Create a quadratic form's matrix to represent a successive-difference variance estimator}
\usage{
make_sd_matrix(n, f = 0, type = "SD1")
}
\arguments{
\item{n}{Number of rows or columns for the matrix}

\item{f}{A single number between \code{0} and \code{1},
representing the sampling fraction. Default value is \code{0}.}

\item{type}{Either "SD1" or "SD2". See the "Details" section for definitions.}
}
\value{
A matrix of dimension \code{n}
}
\description{
A successive-difference variance estimator can be represented
as a quadratic form. This function determines the matrix of the quadratic form.
}
\details{
Ash (2014) describes each estimator as follows:
\deqn{
  \hat{v}_{SD1}(\hat{Y}) = (1-f) \frac{n}{2(n-1)} \sum_{k=2}^n\left(\breve{y}_k-\breve{y}_{k-1}\right)^2
}
\deqn{
  \hat{v}_{SD2}(\hat{Y}) = \frac{1}{2}(1-f)\left[\sum_{k=2}^n\left(\breve{y}_k-\breve{y}_{k-1}\right)^2+\left(\breve{y}_n-\breve{y}_1\right)^2\right]
}
where \eqn{\breve{y}_k} is the weighted value \eqn{y_k/\pi_k} of unit \eqn{k}
with selection probability \eqn{\pi_k}, and \eqn{f} is the sampling fraction \eqn{\frac{n}{N}}.
}
\references{
Ash, S. (2014). "\emph{Using successive difference replication for estimating variances}."
\strong{Survey Methodology}, Statistics Canada, 40(1), 47-59.
}
\keyword{internal}
