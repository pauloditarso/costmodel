createProviders <- function(numberOfProviders, numberOfHosts = 0, numberOfLinks = 0, numberOfNEs = 0) {

	auxEnv <- new.env()

	if (numberOfProviders > 0) {
	  
	  if ( missing(numberOfHosts) ) { numberOfHosts <- sample(2:8, size=1) }
	  if ( missing(numberOfLinks) ) { numberOfLinks <- sample(0:3, size=1) }
	  if ( missing(numberOfNEs) ) { numberOfNEs <- sample(0:3, size=1) }
	  
		for (i in 1:numberOfProviders) {
			name <- paste("p", i, sep = "")
			assign(name, createOneProvider(numberOfHosts, numberOfLinks, numberOfNEs), envir = auxEnv)
		}

		providersNames <- ls(pattern="p", envir = auxEnv)
		providers <- lapply(providersNames, get, env=auxEnv)
		names(providers) <- providersNames

		return(providers)
	}
	else {
		print("ERROR: invalid number of providers!!!")
	}
}
