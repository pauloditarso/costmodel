createProviders <- function(numberOfProviders) {

	auxEnv <- new.env()

	if (numberOfProviders > 0) {
		for (i in 1:numberOfProviders) {
			name <- paste("p", i, sep = "")
			assign(name, createOneProvider(), envir = auxEnv)
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
