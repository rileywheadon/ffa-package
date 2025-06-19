# Generates an informational message about a test
test_message <- function(name, reject, p_value, alpha, msg_fail, msg_reject) {

	s1 <- ifelse(reject, "reject", "fail to reject")
	s2 <- ifelse(reject, msg_reject, msg_fail) 

	l1 <- sprintf("The %s test had a p-value of %f.", name, round(p_value, 3))
	l2 <- sprintf("At a significance level of %f, we %s the null hypothesis.", alpha, s1)
	l3 <- sprintf("Therefore, there is %s.", s2)

	# Combine the messages and add bullet points
	paste0("\n - ", c(l1, l2, l3), collapse = "")

}


# Adds sensible axis scales to a plot
add_scales <- function(p) {
	p + 
	scale_x_continuous(breaks = function(limits) pretty(limits, n = 10)) +
	scale_y_continuous(breaks = function(limits) pretty(limits, n = 10))
}


# Adds sensible styling to a plot
add_theme <- function(p) {
	p +
	theme_minimal() +
	theme(
		plot.background = element_rect(fill = "white", color = NA),
		plot.title = element_text(size = 20, hjust = 0.5),
		plot.margin = margin(5, 15, 5, 15),
		axis.title = element_text(size = 16),
		axis.text = element_text(size = 12),
		panel.grid.minor = element_blank(),
		legend.title = element_text(hjust = 0.5),
		legend.background = element_rect(fill = "white", color = "black"),
		legend.box.background = element_rect(color = "black"),
		legend.direction = "vertical"
	)
}


# Adds a nicely formatted annotation to the top right corner of a plot
add_annotation <- function(p, label) {

	grob <- textGrob(label)
	rect_w <- 0.22
	rect_h <- 0.04

	p + annotation_custom(
		grob = grobTree(
			rectGrob(
				x = 0.95 - (rect_w / 2), y = 0.95,
				hjust = 0.5, vjust = 0.5,
				width = rect_w,
				height = rect_h,
				gp = gpar(fill = "white", col = "black")
			),
			textGrob(
				label,
				x = 0.95 - (rect_w / 2), y = 0.95,
				hjust = 0.5, vjust = 0.5 ,
				gp = gpar(col = "black", fontsize = 10)
			)
		),
		xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf
	)

}

# Helper function for getting information about models
models.info <- function(model) {

	switch(
		model,	
		"GUM" = list(n.params = 2, log = FALSE),
		"NOR" = list(n.params = 2, log = FALSE),
		"LNO" = list(n.params = 2, log =  TRUE),
		"GEV" = list(n.params = 3, log = FALSE, k.bounds = c(-9, 9)),
		"GLO" = list(n.params = 3, log = FALSE, k.bounds = c(-0.999, 0.999)),
		"PE3" = list(n.params = 3, log = FALSE, k.bounds = c(-10, 10)),
		"LP3" = list(n.params = 3, log =  TRUE, k.bounds = c(-10, 10)),
		"GNO" = list(n.params = 3, log = FALSE, k.bounds = c(-4, 4)),
		"WEI" = list(n.params = 3, log = FALSE, k.bounds = c(-9, 9)),
		"GUM10" = list(n.params = 3),
		"GUM11" = list(n.params = 4),
		"NOR10" = list(n.params = 3),
		"NOR11" = list(n.params = 4),
		"LNO10" = list(n.params = 3),
		"LNO11" = list(n.params = 4),
		"GEV100" = list(n.params = 4),
		"GEV110" = list(n.params = 5),
		"GLO100" = list(n.params = 4),
		"GLO110" = list(n.params = 5),
		"GNO100" = list(n.params = 4),
		"GNO110" = list(n.params = 5),
		"PE3100" = list(n.params = 4),
		"PE3110" = list(n.params = 5),
		"LP3100" = list(n.params = 4),
		"LP3110" = list(n.params = 5),
		"WEI100" = list(n.params = 4),
		"WEI110" = list(n.params = 5)
	)

}

# Helper function for computing covariates. A covariate is a normalized version of 
# the year where 0 corresponds to the first year and 1 is the last year. Covariates
# are used to fit parameters for non-stationary distributions.
get.covariates <- function(years) (years - 1900) / 100

# # Helper function for validaitng parameters passed to a model
# check.parameters <- function(model, params) {

# 	# List of models with the number of parameters
# 	models.info <- list(
# 		"GUM"    = 2,
# 		"GUM10"  = 3,
# 		"GUM11"  = 4,
# 		"NOR"    = 2,
# 		"NOR10"  = 3,
# 		"NOR11"  = 4,
# 		"LNO"    = 2,
# 		"LNO10"  = 3,
# 		"LNO11"  = 4,
# 		"GEV"    = 3,
# 		"GEV100" = 4,
# 		"GEV110" = 5,
# 		"GLO"    = 3,
# 		"GLO100" = 4,
# 		"GLO110" = 5,
# 		"GNO"    = 3,
# 		"GNO100" = 4,
# 		"GNO110" = 5,
# 		"PE3"    = 3,
# 		"PE3100" = 4,
# 		"PE3110" = 5,
# 		"LP3"    = 3,
# 		"LP3100" = 4,
# 		"LP3110" = 5,
# 		"WEI"    = 3,
# 		"WEI100" = 4,
# 		"WEI110" = 5
# 	)

# 	# Check that params has the correct number of entries
# 	if (length(params) != models.info[[model]]) {
# 		str <- "'params' for model '%s' must have length %d."
# 		result <- list(
# 			success = FALSE,
# 			msg = sprintf(str, model, models.info[[model]])
# 		)
# 	} else {
# 		result <- list(success = TRUE)
# 	}

# 	return (result)


# }
