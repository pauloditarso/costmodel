createLinks <- function(numberOfLinks, minLinks, env) {
	# still needs a pricing definition

	if ( numberOfLinks > 0 && is.environment(env) ) {
	  
	  if (minLinks$cap == 4) { capacity <- rep(4, numberOfLinks) }
	  else {
	    auxCap <- vector()
	    for (i in c(1, 2, 3, 4)) { if(i >= minLinks$cap) auxCap <- c(auxCap, i) }
	    capacity <- sort(sample(auxCap, size = numberOfLinks, replace = TRUE))
	  }
	  
	  if (minLinks$del == 4) { delay <- rep(4, numberOfLinks) }
	  else {
	    auxDel <- vector()
	    for (i in c(1, 2, 3, 4)) { if(i >= minLinks$del) auxDel <- c(auxDel, i) }
	    delay <- sort(sample(auxDel, size = numberOfLinks, replace = TRUE))
	  }
	  
	  if (minLinks$jit == 4) { jitter <- rep(4, numberOfLinks) }
	  else {
	    auxJit <- vector()
	    for (i in c(1, 2, 3, 4)) { if(i >= minLinks$jit) auxJit <- c(auxJit, i) }
	    jitter <- sort(sample(auxJit, size = numberOfLinks, replace = TRUE))
	  }
	  
		# capacity <- sort(sample(c(1, 2, 3, 4), size=numberOfLinks, replace=TRUE))
		# delay <- sort(sample(c(1, 2, 3, 4), size=numberOfLinks, replace=TRUE))
		# jitter <- sort(sample(c(1, 2, 3, 4), size=numberOfLinks, replace=TRUE))

		for (i in 1:numberOfLinks) {
			name <- paste("link", i, sep="")
			assign(name, c('cap' = capacity[i], 'del' = delay[i], 'jit' = jitter[i]), envir = env)
		}
	}
	else {
		print("ERROR: invalid number of links or env does not exist!!!")
	}

}
