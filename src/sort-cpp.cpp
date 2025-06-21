#include <Rcpp.h>
#include <algorithm>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector sort_cpp(NumericVector x) {
	NumericVector result = clone(x);  // avoid modifying input
	std::sort(result.begin(), result.end());
	return result;
}

