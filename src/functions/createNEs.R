createNEs <- function(numberOfNEs, env) {
	# still needs a pricing definition

	if ( numberOfNEs > 0 && is.environment(env) ) {
		capacity <- sort(sample(c(1, 2, 3, 4), size=numberOfNEs, replace=TRUE))
		ports <- sort(sample(c(6, 12, 24, 48), size=numberOfNEs, replace=TRUE))
		queue <- sort(sample(c(1, 2, 3, 4), size=numberOfNEs, replace=TRUE))

		for (i in 1:numberOfNEs) {
			name <- paste("ne", i, sep="")
			assign(name, c('cap' = capacity[i], 'por' = ports[i], 'que' = queue[i]), envir = env)
		}
	}
	else {
		print("ERROR: invalid number of network elements or env does not exist!!!")
	}

}
