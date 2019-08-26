indexes <- function(numberOfRequests, numberOfResources) {

	library(gtools)

	requests <- permutations(numberOfRequests, numberOfRequests)
	resources <- combinations(numberOfResources, numberOfRequests)

	if ( ncol(requests) > nrow(resources) ) return -1

	allCombinations <- data.frame()

	for ( i in 1:nrow(requests) ) {

		for ( j in 1:nrow(resources) ) {

			aux <- c(requests[i,], resources[j,])
			allCombinations <- rbind(allCombinations, aux)

		}

	}

	rm(i,j)
	allCombinations

}