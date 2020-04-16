indexes <- function(numberOfRequests, numberOfResources) {
  
  allCombinations <- data.frame()
  count <- 0
  
  if ( numberOfRequests == 0 | (numberOfRequests > numberOfResources) ) {
    allCombinations
  }
  else {
    requests <- gtools::permutations(numberOfRequests, numberOfRequests)
    resources <- gtools::combinations(numberOfResources, numberOfRequests)
    
    for ( i in 1:nrow(requests) ) {
      
      for ( j in 1:nrow(resources) ) {
        count <- count + 1
        aux <- c(requests[i,], resources[j,])
        allCombinations <- rbind(allCombinations, aux)
        
        if (count == 9000) {
          break
        } 
      }
      if (count == 9000) { break }
    }
    
    rm(i,j)
    allCombinations
  }

}