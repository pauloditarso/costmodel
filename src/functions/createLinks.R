createLinks <- function(numberOfLinks, env) {
	# still needs a pricing definition

	if ( numberOfLinks > 0 && is.environment(env) ) {
		capacity <- sort(sample(c(1, 2, 3, 4), size=numberOfLinks, replace=TRUE))
		delay <- sort(sample(c(1, 2, 3, 4), size=numberOfLinks, replace=TRUE))
		jitter <- sort(sample(c(1, 2, 3, 4), size=numberOfLinks, replace=TRUE))

		for (i in 1:numberOfLinks) {
			name <- paste("link", i, sep="")
			assign(name, c('cap' = capacity[i], 'del' = delay[i], 'jit' = jitter[i]), envir = env)
		}
	}
	else {
		print("ERROR: invalid number of links or env does not exist!!!")
	}

}
