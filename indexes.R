indexes <- function(numberOfRequests, numberOfResources) {

  if (numberOfRequests == 0 | numberOfRequests > numberOfResources) return -1
  
	#library(gtools)

	requests <- gtools::permutations(numberOfRequests, numberOfRequests)
	resources <- gtools::combinations(numberOfResources, numberOfRequests)
	
	#if ( ncol(requests) > nrow(resources) ) return -1
	
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