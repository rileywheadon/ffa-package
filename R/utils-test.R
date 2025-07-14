# Generates an informational message about a statistical test
stats_message <- function(name, reject, p_value, alpha, msg_fail, msg_reject) {

	p_text <- if (p_value < 0.001) "<0.001" else as.character(round(p_value, 3))

	s1 <- if(reject) "reject the null" else "fail to reject the null"
	s2 <- if(reject) msg_reject else msg_fail

	l1 <- sprintf("The %s test had a p-value of %s.", name, p_text)
	l2 <- sprintf("At a significance level of %.2f, we %s hypothesis.", alpha, s1)
	l3 <- sprintf("Therefore, there is %s.", s2)

	# Combine the messages and add bullet points
	paste0("\n - ", c(l1, l2, l3), collapse = "")

}



