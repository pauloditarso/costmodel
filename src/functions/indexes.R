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
    
    # for ( i in 1:nrow(requests) ) {
    #   
    #   for ( j in 1:nrow(resources) ) {
    #     aux <- c(requests[i,], resources[j,])
    #     allCombinations <- rbind(allCombinations, aux)
    #     
    #   }
    # 
    # }
    
    count <- 1
    occurrences <- data.frame(i=character(0), j=character(0))
    condition <- min(totalLength, quota)
    
    while (count <= condition) {
      i <- sample(nrow(requests), 1)
      j <- sample(nrow(resources), 1)
      auxDF <- data.frame(i, j)
      
      if ( duplicated(rbind(occurrences, auxDF))[nrow(occurrences)+1] ) {
        next
      }
      aux <- c(requests[i,], resources[j,])
      allCombinations <- rbind(allCombinations, aux)
      occurrences <- rbind(occurrences, auxDF)
      count <- count + 1
    }
    
    rm(i,j)
    allCombinations
    # if ( quota == -1 ) {
    #   allCombinations
    # }
    # else {
    #   allCombinations[c(sample(nrow(allCombinations),min(quota,totalLength),replace = FALSE)),]
    # }

  }

}