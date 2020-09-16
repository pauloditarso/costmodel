createNEs <- function(numberOfNEs, minNEs, env) {
	# still needs a pricing definition

	if ( numberOfNEs > 0 && is.environment(env) ) {
	  
	  if (minNEs$cap == 4) { capacity <- rep(4, numberOfNEs) }
	  else {
	    auxCap <- vector()
	    for (i in c(1, 2, 3, 4)) { if(i >= minNEs$cap) auxCap <- c(auxCap, i) }
	    capacity <- sort(sample(auxCap, size = numberOfNEs, replace = TRUE))
	  }
	  
	  if (minNEs$por == 48) { ports <- rep(48, numberOfNEs) }
	  else {
	    auxPor <- vector()
	    for (i in c(6, 12, 24, 48)) { if(i >= minNEs$por) auxPor <- c(auxPor, i) }
	    ports <- sort(sample(auxPor, size = numberOfNEs, replace = TRUE))
	  }
	  
	  if (minNEs$que == 4) { queue <- rep(4, numberOfNEs) }
	  else {
	    auxQue <- vector()
	    for (i in c(1, 2, 3, 4)) { if(i >= minNEs$que) auxQue <- c(auxQue, i) }
	    queue <- sort(sample(auxQue, size = numberOfNEs, replace = TRUE))
	  }
	  
		# capacity <- sort(sample(c(1, 2, 3, 4), size=numberOfNEs, replace=TRUE))
		# ports <- sort(sample(c(6, 12, 24, 48), size=numberOfNEs, replace=TRUE))
		# queue <- sort(sample(c(1, 2, 3, 4), size=numberOfNEs, replace=TRUE))

		for (i in 1:numberOfNEs) {
			name <- paste("ne", i, sep="")
			assign(name, c('cap' = capacity[i], 'por' = ports[i], 'que' = queue[i]), envir = env)
		}
	}
	else {
		print("ERROR: invalid number of network elements or env does not exist!!!")
	}

}
