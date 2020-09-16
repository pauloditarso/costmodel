createOneProvider <- function(numberOfHosts = 0, numberOfLinks = 0, numberOfNEs = 0, minHosts, minLinks, minNEs) {
	# there is no treatment for provider profiles
	# also, still needs a pricing definition

	providerEnv <- new.env()

	# if ( missing(numberOfHosts) ) { numberOfHosts <- sample(0:10, size=1) }
	# if ( missing(numberOfLinks) ) { numberOfLinks <- sample(0:5, size=1) }
	# if ( missing(numberOfNEs) ) { numberOfNEs <- sample(0:5, size=1) }

	if (numberOfHosts > 0) {
		createHosts(numberOfHosts, minHosts, providerEnv)
	}
	else {
		assign("host1", 0, envir=providerEnv)
	}

	if (numberOfLinks > 0) {
		createLinks(numberOfLinks, minLinks, providerEnv)
	}
	else {
		assign("link1", 0, envir=providerEnv)
	}

	if (numberOfNEs > 0) {
		createNEs(numberOfNEs, minNEs, providerEnv)
	}
	else {
		assign("ne1", 0, envir=providerEnv)
	}

	hosts <- lapply(ls(providerEnv, pattern="host"), get, env=providerEnv)
	links <- lapply(ls(providerEnv, pattern="link"), get, env=providerEnv)
	nes <- lapply(ls(providerEnv, pattern="ne"), get, env=providerEnv)

	provider <- list(hosts, links, nes)
	names(provider) <- c("hosts", "links", "nes")
	return(provider)

}
