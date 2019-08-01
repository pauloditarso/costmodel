createHosts <- function(numberOfHosts, env) {
	# still needs a pricing definition

	if ( numberOfHosts > 0 && is.environment(env) ) {
		cpu <- sort(sample(c(1, 2, 4, 8, 16, 32), size=numberOfHosts, replace=TRUE))
		mem <- sort(sample(c(4, 8, 16, 32, 64, 128), size=numberOfHosts, replace=TRUE))
		str <- sort(sample(c(128, 256, 512, 1024, 2048, 4096), size=numberOfHosts, replace=TRUE))

		for (i in 1:numberOfHosts) {
			auxName <- paste("host", i, sep="")
			assign(auxName, c('cpu' = cpu[i], 'men' = mem[i], 'str' = str[i]), envir = env)
		}
	}
	else {
		print("ERROR: invalid number of hosts or env does not exist!!!")
	}

}
