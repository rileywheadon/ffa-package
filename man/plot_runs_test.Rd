% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-runs-test.R
\name{plot_runs_test}
\alias{plot_runs_test}
\title{Plot Runs Test Results}
\usage{
plot_runs_test(results, type, ...)
}
\arguments{
\item{results}{A list of runs test results generated by \link{eda_runs_test}.}

\item{type}{A character scalar representing whether Sen's trend estimator was
applied directly to the AMS data (\code{"mean"}) or to the moving-window variance
of the data (\code{"variance"}). Must be either \code{"variance"} or \code{"mean"}.}

\item{...}{Optional named arguments: 'title', 'xlabel', and 'ylabel'.}
}
\value{
\code{ggplot}; a plot containing:
\itemize{
\item Black points for the residual at each year.
\item A red dashed horizontal line at \eqn{y = 0}.
\item A text annotation “Runs : X.XXX” in the plot area.
}
}
\description{
Generates a residual plot of Sen’s estimator applied to annual maximum streamflow
(AMS) data with a horizontal dashed line at zero and an annotation of the Runs
test .
}
\examples{
# Initialize data and years
data <- rnorm(n = 100, mean = 100, sd = 10)
years <- seq(from = 1901, to = 2000)

# Generate runs test plot 
sens <- eda_sens_trend(data, years)
results <- eda_runs_test(sens)
plot_runs_test(results, "mean")

}
\seealso{
\link{eda_runs_test}
}
