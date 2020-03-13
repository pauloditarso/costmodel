indexes <- function(numberOfRequests, numberOfResources) {
  
  allCombinations <- data.frame()
  
  if ( numberOfRequests == 0 | (numberOfRequests > numberOfResources) ) {
    allCombinations
  }
  else {
    requests <- gtools::permutations(numberOfRequests, numberOfRequests)
    resources <- gtools::combinations(numberOfResources, numberOfRequests)
    
    for ( i in 1:nrow(requests) ) {
      
      for ( j in 1:nrow(resources) ) {
        
        aux <- c(requests[i,], resources[j,])
        allCombinations <- rbind(allCombinations, aux)
        
      }
      
    }
    
    rm(i,j)
    allCombinations
  }

}