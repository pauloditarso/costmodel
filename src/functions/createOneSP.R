createOneSP <- function(numberOfHosts = 0, numberOfLinks = 0, numberOfNEs = 0) {
	# there is no treatment for SP profiles
	# also, still needs a pricing definition

	spEnv <- new.env()

	if ( missing(numberOfHosts) ) { numberOfHosts <- sample(2:8, size=1) }
	if ( missing(numberOfLinks) ) { numberOfLinks <- sample(0:3, size=1) }
	if ( missing(numberOfNEs) ) { numberOfNEs <- sample(0:3, size=1) }

	# number of hosts is always greater than 1
	minHosts <- data.frame("cpu" = 1, "mem" = 4, "str" = 128)
	createHosts(numberOfHosts, minHosts, spEnv)

	if (numberOfLinks > 0) {
	  minLinks <- data.frame("cap" = 1, "del" = 1, "jit" = 1)
		createLinks(numberOfLinks, minLinks, spEnv)
	}
	else {
		assign("link1", 0, envir=spEnv)
	}

	if (numberOfNEs > 0) {
	  minNEs <- data.frame("cap" = 1, "por" = 6, "que" = 1)
		createNEs(numberOfNEs, minNEs, spEnv)
	}
	else {
		assign("ne1", 0, envir=spEnv)
	}

	hosts <- lapply(ls(spEnv, pattern="host"), get, env=spEnv)
	links <- lapply(ls(spEnv, pattern="link"), get, env=spEnv)
	nes <- lapply(ls(spEnv, pattern="ne"), get, env=spEnv)

	SP <- list(hosts, links, nes)
	names(SP) <- c("hosts", "links", "nes")
	return(SP)

}