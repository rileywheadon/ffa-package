% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eda-bbmk-test.R
\name{eda_bbmk_test}
\alias{eda_bbmk_test}
\title{Block-Bootstrap Mann-Kendall Test for Trend Detection}
\usage{
eda_bbmk_test(data, alpha = 0.05, samples = 10000L, quiet = TRUE)
}
\arguments{
\item{data}{Numeric vector of annual maximum streamflow (AMS) values.
Must be strictly positive, finite, and non-missing.}

\item{alpha}{Numeric scalar in \eqn{[0.01, 0.1]}. The significance
level for confidence intervals or hypothesis tests. Default is 0.05.}

\item{samples}{Integer scalar. The number of bootstrap samples. Default is 10000.}

\item{quiet}{Logical scalar. If \code{FALSE}, prints a summary of of the statistical
test to the console. Default is \code{TRUE}.}
}
\value{
List; the results of the test, including:
\itemize{
\item \code{data}: The \code{data} argument.
\item \code{s_bootstrap}: Vector of bootstrapped test statistics used for plotting.
\item \code{s_statistic}: The Mann-Kendall test statistic computed on the original series.
\item \code{p_value}: Empirical two-sided p-value computed from the bootstrap distribution.
\item \code{bounds}: Confidence interval bounds for the null distribution of the statistic.
\item \code{reject}: Logical. If \code{TRUE}, the null hypothesis was rejected at \code{alpha}.
\item \code{msg}: Character string summarizing the test result (printed if \code{quiet = FALSE}).
}
}
\description{
Performs a bootstrapped version of the Mann-Kendall trend test to account
for serial correlation in annual maximum streamflow (AMS) data. The procedure
uses Spearman’s autocorrelation test to estimate the least insignificant lag,
then applies a bootstrap procedure to obtain the empirical p-value and confidence
bounds for the Mann-Kendall S-statistic.
}
\details{
The block size for the bootstrap is selected as \code{least_lag + 1}, where \code{least_lag}
is estimated using \link{eda_spearman_test}. Each bootstrap sample is generated by
resampling blocks of the original data  (without replacement) and computing the
Mann-Kendall S-statistic. This procedure adjusts for autocorrelation in the data.
}
\examples{
data <- rnorm(n = 100, mean = 100, sd = 10)
eda_bbmk_test(data, samples = 1000L)

}
\seealso{
\link{plot_bbmk_test}, \link{eda_mk_test}, \link{eda_spearman_test}
}
