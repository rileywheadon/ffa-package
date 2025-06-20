% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sb-uncertainty.R
\name{sb.uncertainty}
\alias{sb.uncertainty}
\title{Sample Bootstrap Confidence Intervals for Flood Quantile Estimates}
\usage{
sb.uncertainty(
  data,
  years,
  model,
  method,
  slices = "last",
  n_sim = 10000,
  alpha = 0.05,
  parallel = FALSE,
  prior = NULL
)
}
\arguments{
\item{years}{Character string or numeric vector specifying the years at which
to compute the estimates and confidence intervals. Defaults to "last".
\itemize{
\item{\code{"all"} returns estimates for all years in the dataset.}
\item{\code{"first"} returns estimates for first year in the dataset.}
\item{\code{"last"} returns estimates for last year in the dataset.}
\item{Passing a numeric vector to \code{years} allows for custom values.}
}
If the chosen model is stationary, the results will be the same for all years}

\item{model}{Character string specifying the distribution code. The first three letters
denote the family: 'GUM', 'NOR', 'LNO', 'GEV', 'GLO', 'GNO', 'PE3', 'LP3', or 'WEI'.
A trailing signature of '10' or '100' indicates a linear trend in location; '11' or
'110' indicates linear trends in both location and scale.}

\item{method}{Character string specifying the estimation method.
Currently supports \code{"L-moments"}, \code{"MLE"}, and \code{"GMLE"}.}

\item{n_sim}{Integer number of bootstrap simulations (default is 100000).}

\item{alpha}{Numeric significance level for the confidence intervals (default is 0.05).}

\item{parallel}{Logical. If TRUE, runs the bootstrap in parallel (default is FALSE).}

\item{df}{Dataframe with columns "max", a vector of annual maxima observations,
and "year", a vector of years corresponding to the observations in "max".}
}
\value{
A named list containing a list of years. Each year maps to a sublist:
\describe{
\item{estimates}{Vector of estimated quantiles for return periods 2, 5, 10, 20, 50, and 100.}
\item{ci_lower}{Lower bound of the confidence interval for each return period.}
\item{ci_upper}{Upper bound of the confidence interval for each return period.}
\item{t}{Vector of return periods (2, 5, 10, 20, 50, and 100).}
}
}
\description{
Computes confidence intervals for flood quantile estimates using the nonparametric
sample bootstrap method, based on L-moment parameter estimation. This function supports
uncertainty quantification for return period estimates derived from a fitted distribution.
}
\details{
The bootstrap procedure simulates resamples from the fitted distribution via inverse transform
sampling using the estimated parameters. For each resample, L-moment parameters are re-estimated
and used to compute quantiles. Confidence intervals are obtained by applying empirical quantiles
to the resulting distribution of estimates.

Using \code{parallel = TRUE} can reduce computation time by approximately 50\%.
However, using this option will nullify any calls to \code{set.seed()},
so your results may not be reproducible.
}
\seealso{
\code{\link[lmom]{samlmu}}, \code{\link[stats]{quantile}}
}
