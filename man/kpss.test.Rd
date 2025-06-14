% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kpss-test.R
\name{kpss.test}
\alias{kpss.test}
\title{Kwiatkowski–Phillips–Schmidt–Shin (KPSS) Unit Root Test}
\usage{
kpss.test(ams, alpha = 0.05, quiet = TRUE)
}
\arguments{
\item{ams}{A numeric vector of annual maximum streamflow data. Must not contain NA values.}

\item{alpha}{A numeric value indicating the significance level. Must be 0.01, 0.05, or 0.10.}

\item{quiet}{Logical. If FALSE, prints a summary message to the console (default is TRUE).}
}
\value{
A named list with the following components:
\describe{
\item{p.value}{The reported p-value from the test. See notes regarding discrete thresholds.}
\item{reject}{Logical. TRUE if the null hypothesis of stationarity is rejected at \code{alpha}.}
\item{msg}{Character string summarizing the test outcome, printed if \code{quiet = FALSE}.}
}
}
\description{
Performs the KPSS test for stationarity in annual maximum streamflow (AMS) data using the
\pkg{aTSA} package. The null hypothesis is that the time series is stationary.
}
\details{
The KPSS test implementation in \pkg{aTSA} uses interpolation from the Hobjin et al. (2004)
significance table, which only includes thresholds for 0.01, 0.05, and 0.10. As such, the
returned p-values are discretized. Specifically, \eqn{p = 0.01} implies \eqn{p \leq 0.01},
and \eqn{p = 0.10} implies \eqn{p \geq 0.10}. We use the Type III KPSS test, which accounts
for drift and a nonstationary trend in the data by fitting an auxillary model.
}
\seealso{
\code{\link[aTSA]{kpss.test}}, \code{\link{pp.test}}
}
