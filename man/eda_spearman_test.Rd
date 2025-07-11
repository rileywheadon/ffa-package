% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eda-spearman-test.R
\name{eda_spearman_test}
\alias{eda_spearman_test}
\title{Spearman Test for Autocorrelation}
\usage{
eda_spearman_test(data, alpha = 0.05, quiet = TRUE)
}
\arguments{
\item{data}{Numeric vector of annual maximum streamflow (AMS) values.
Must be strictly positive, finite, and non-missing.}

\item{alpha}{Numeric scalar in \eqn{[0.01, 0.1]}. The significance
level for confidence intervals or hypothesis tests. Default is 0.05.}

\item{quiet}{Logical scalar. If \code{FALSE}, prints a summary of of the statistical
test to the console. Default is \code{TRUE}.}
}
\value{
A list containing the test results, including:
\itemize{
\item \code{data}: The \code{data} argument.
\item \code{rho}: Numeric vector of autocorrelation estimates for lags \eqn{1} to \eqn{n-3}.
\item \code{sig}: Logical vector indicating which lags exhibit significant autocorrelation.
\item \code{least.lag}: The smallest lag at which the autocorrelation is insignificant.
\item \code{reject}: Logical. If \code{TRUE}, then \code{least.lag > 0}.
\item \code{msg}: Character string summarizing the test result, printed if \code{quiet = FALSE}.
}
}
\description{
Performs the Spearman rank autocorrelation test on annual maximum streamflow
(AMS) data to check for autocorrelation at various lags. Reports the smallest
lag where the autocorrelation is not statistically significant at the given
significance level.
}
\details{
For each lag from \eqn{1} to \eqn{n - 3}, the function computes the Spearman
correlation coefficient between the AMS series and its lagged version. The
first lag with an insignificant autocorrelation coefficient returned as \code{least.lag}.
}
\examples{
data <- rnorm(n = 100, mean = 100, sd = 10)
eda_spearman_test(data)

}
\seealso{
\link[stats]{cor.test}, \link{eda_bbmk_test}, \link{plot_spearman_test}
}
