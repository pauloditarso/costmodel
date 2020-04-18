indexes <- function(numberOfRequests, numberOfResources, quota) {
  
  allCombinations <- data.frame()
  
  if ( numberOfRequests == 0 | (numberOfRequests > numberOfResources) ) {
    allCombinations
  }
  else {
    
    totalPermutaions <- factorial(numberOfRequests)
    totalCombinations <- ( factorial(numberOfResources) /( totalPermutaions*(factorial(numberOfResources-numberOfRequests)) ) )
    totalLength <- ( totalPermutaions * totalCombinations )
    
    requests <- gtools::permutations(numberOfRequests, numberOfRequests)
    resources <- gtools::combinations(numberOfResources, numberOfRequests)
    
    for ( i in 1:nrow(requests) ) {
      
      for ( j in 1:nrow(resources) ) {
        aux <- c(requests[i,], resources[j,])
        allCombinations <- rbind(allCombinations, aux)
        
      }
  
    }
    
    rm(i,j)
    # allCombinations
    if ( quota == -1 ) {
      allCombinations
    }
    else {
      allCombinations[c(sample(nrow(allCombinations),min(quota,totalLength),replace = FALSE)),]
    }

  }

}