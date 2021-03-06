% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utilities.r
\name{vcov.clme}
\alias{vcov.clme}
\alias{vcov.summary.clme}
\title{Variance-covariance matrix}
\usage{
\method{vcov}{clme}(object, ...)

\method{vcov}{summary.clme}(object, ...)
}
\arguments{
\item{object}{object of class \code{\link{clme}}.}

\item{...}{space for additional arguments}
}
\value{
Numeric matrix.
}
\description{
Extracts variance-covariance matrix for objects of class \code{clme}.

Variance-covariance matrix
}
\examples{
data( rat.blood )
cons <- list(order = "simple", decreasing = FALSE, node = 1 )
clme.out <- clme(mcv ~ time + temp + sex + (1|id), data = rat.blood ,
                 constraints = cons, seed = 42, nsim = 0)

vcov( clme.out )
}
\seealso{
\code{\link{CLME-package}}
\code{\link{clme}}
}

