createHosts <- function(numberOfHosts, minHosts, env) {
	# still needs a pricing definition

	if ( numberOfHosts > 0 && is.environment(env) ) {
	  
	  if (minHosts$cpu == 32) { cpu <- rep(32, numberOfHosts) }
	  else {
	    auxCpu <- vector()
	    for (i in c(1, 2, 4, 8, 16, 32)) { if(i >= minHosts$cpu) auxCpu <- c(auxCpu, i) }
	    cpu <- sort(sample(auxCpu, size = numberOfHosts, replace = TRUE))
	  }

	  if (minHosts$mem == 128) { mem <- rep(128, numberOfHosts) }
	  else {
	    auxMem <- vector()
	    for (i in c(4, 8, 16, 32, 64, 128)) { if(i >= minHosts$mem) auxMem <- c(auxMem, i) }
	    mem <- sort(sample(auxMem, size = numberOfHosts, replace = TRUE))
	  }
	  
	  if (minHosts$str == 4096) { str <- rep(32, numberOfHosts) }
	  else {
	    auxStr <- vector()
	    for (i in c(128, 256, 512, 1024, 2048, 4096)) { if(i >= minHosts$str) auxStr <- c(auxStr, i) }
	    str <- sort(sample(auxStr, size = numberOfHosts, replace = TRUE))
	  }
	  
		# cpu <- sort(sample(c(1, 2, 4, 8, 16, 32), size=numberOfHosts, replace=TRUE))
		# mem <- sort(sample(c(4, 8, 16, 32, 64, 128), size=numberOfHosts, replace=TRUE))
		# str <- sort(sample(c(128, 256, 512, 1024, 2048, 4096), size=numberOfHosts, replace=TRUE))

		for (i in 1:numberOfHosts) {
			auxName <- paste("host", i, sep="")
			assign(auxName, c('cpu' = cpu[i], 'mem' = mem[i], 'str' = str[i]), envir = env)
		}
	}
	else {
		print("ERROR: invalid number of hosts or env does not exist!!!")
	}

}
