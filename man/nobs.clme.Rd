% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/utilities.r
\name{nobs.clme}
\alias{nobs.clme}
\alias{nobs.summary.clme}
\title{Number of observations}
\usage{
\method{nobs}{clme}(object, ...)

\method{nobs}{summary.clme}(object, ...)
}
\arguments{
\item{object}{an object of class \code{clme}.}

\item{...}{space for additional arguments}
}
\value{
Numeric.
}
\description{
Obtains the number of observations used to fit an model for objects of class \code{clme}.

Number of observations
}
\examples{
data( rat.blood )
cons <- list(order = "simple", decreasing = FALSE, node = 1 )
clme.out <- clme(mcv ~ time + temp + sex + (1|id), data = rat.blood ,
                 constraints = cons, seed = 42, nsim = 0)

nobs( clme.out )
}
\seealso{
\code{\link{CLME-package}}
\code{\link{clme}}

\code{\link{nobs.clme}}
}

