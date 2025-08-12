# Trends to use for unit tests
S00 <- list(location = F, scale = F)
S10 <- list(location = T, scale = F)
S01 <- list(location = F, scale = T)
S11 <- list(location = T, scale = T)

# Default options for testing submodules
default_options <- list(
	significance_level = 0.05,
	bbmk_samples = 10000L,
	window_size = 10L,
	window_step = 5L,
	selection = "L-distance",
	z_samples = 10000L,
	s_estimation = "L-moments",
	ns_estimation = "MLE",
	gev_prior = c(6, 9),
	s_uncertainty = "Bootstrap",
	ns_uncertainty = "RFPL",
	return_periods = c(2L, 5L, 10L, 20L, 50L, 100L),
	ns_slices = c(1925L, 1975L, 2025L),
	bootstrap_samples = 2000L,
	rfpl_tolerance = 0.01,
	pp_formula = "Weibull"
)
