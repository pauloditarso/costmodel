experiments <- function(numberOfProviders, numberOfTurns) {

  providers <- numberOfProviders
  turns <- numberOfTurns
  results <- vector()
  
  for ( i in 1:turns) {
    
    valid <- FALSE
    count <- 0
    
    while(!valid)  {
      
      source('~/Workspace/R-workspace/costmodel/start.R')
      
      if ( failed ) {
        count <- count + 1
      }
      else {
        valid <- TRUE  
      }
      
      source('./desourceAll.R')

    }
    
    results[i] <- count
  
  }
  
  auxName <- paste("results_novo_", providers, sep = "")
  assign(auxName, results, envir = .GlobalEnv)
  rm(auxName, results)

}
