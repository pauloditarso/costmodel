rm(list = ls())
source('./src/scripts/sourceAll.R')
SPConfig<-c(2,2,1)
numberOfProviders <- 0
numberOfTurns <- 1000
priceHostPerDay <- 10000
priceLinkPerDay <- 10000
priceNEPerDay <- 10000
pricingType <- "fixed"
  
referenceHost <- c(1, 4, 128, 0.1)
referenceLink <- c(1, 1, 4, 0.2)
referenceNE <- c(1, 6, 1, 0.2)

SP <- createOneSP(SPConfig[1], SPConfig[2], SPConfig[3])
  
SPhosts <- decomposeSP(SP, "hosts")
SPlinks <- decomposeSP(SP, "links")
SPnes <- decomposeSP(SP, "nes")
  
for ( i in 1:10 ) {
  
  numberOfProviders <- i
  results <- vector()
    
  for ( j in 1:numberOfTurns) {
      
    valid <- FALSE
    count <- 0
      
    while(!valid)  {
        
      P <- createProviders(numberOfProviders, 2, 2, 1)
        
      Phosts <- decomposeProv(P, "hosts")
      Plinks <- decomposeProv(P, "links")
      Pnes <- decomposeProv(P, "nes")
        
      hostsComb <- indexes(nrow(SPhosts), nrow(Phosts))
      linksComb <- indexes(nrow(SPlinks), nrow(Plinks))
      nesComb <- indexes(nrow(SPnes), nrow(Pnes))
        
      hostsDF <- hostsAnswers(hostsComb, priceHostPerDay, pricingType)
      linksDF <- linksAnswers(linksComb, priceLinkPerDay, pricingType)
      nesDF <- nesAnswers(nesComb, priceNEPerDay, pricingType)
        
      if ( nrow(hostsDF) == 0 | nrow(linksDF) == 0 | nrow(nesDF) == 0 ) { 
        failed <- TRUE
      } else {
        failed <- FALSE
      }
        
      if ( failed ) {
        count <- count + 1
      }
      else {
        valid <- TRUE  
      }
        
      source('./src/scripts/desourceAll.R')
        
    }
      
    results[j] <- count
      
  }
    
  auxName <- paste("results_", numberOfProviders, sep = "")
  assign(auxName, results, envir = .GlobalEnv)
  rm(auxName, results)
    
}

resultsList <- ls(pattern = "results")
rm(list = setdiff(ls(), resultsList))
save.image()