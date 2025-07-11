% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit-lmom-methods.R
\name{fit_lmom_methods}
\alias{fit_lmom_methods}
\alias{fit_lmom_gum}
\alias{fit_lmom_nor}
\alias{fit_lmom_lno}
\alias{fit_lmom_gev}
\alias{fit_lmom_glo}
\alias{fit_lmom_gno}
\alias{fit_lmom_pe3}
\alias{fit_lmom_lp3}
\alias{fit_lmom_wei}
\title{Parameter Estimation with L-Moments}
\usage{
fit_lmom_gum(data)

fit_lmom_nor(data)

fit_lmom_lno(data)

fit_lmom_gev(data)

fit_lmom_glo(data)

fit_lmom_gno(data)

fit_lmom_pe3(data)

fit_lmom_lp3(data)

fit_lmom_wei(data)
}
\arguments{
\item{data}{Numeric vector of annual maximum streamflow (AMS) values.
Must be strictly positive, finite, and non-missing.}
}
\value{
A numeric vector of parameters:
\itemize{
\item If \code{model} is \code{"GUM"}, \code{"NOR"}, or \code{"LNO"}, returns a vector of length 2.
\item Otherwise, returns a vector of length 3.
}
}
\description{
Estimate the parameters of one of nine different distributions (\code{GUM}, \code{NOR},
\code{LNO}, \code{GEV}, \code{GLO}, \code{GNO}, \code{PE3}, \code{LP3}, and \code{WEI}) using the method of L-moments.
}
\details{
First, the sample L-moments of the data are computed using the \link{lmom_sample}
method. Then formulas from Hosking (1997) are used to compute the parameters from
the L-moments. Distributions \code{GNO}, \code{PE3}, and \code{LP3} use a rational approximation
to compute the parameters.
}
\examples{
data <- rnorm(n = 100, mean = 100, sd = 10)
fit_lmom_lp3(data)

}
\references{
Hosking, J.R.M. & Wallis, J.R., 1997. Regional frequency analysis: an approach based
on L-Moments. Cambridge University Press, New York, USA.
}
\seealso{
\link{fit_lmom_fast}, \link{fit_lmom_kappa}
}
