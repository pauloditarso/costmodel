rm(list = ls())
source('./src/scripts/sourceAll.R')
SPConfig<-c(2,2,1)
numberOfProviders <- 0
numberOfTurns <- 2
priceHostPerDay <- 100000
priceLinkPerDay <- 100000
priceNEPerDay <- 100000
pricingType <- "fixed"

referenceHost <- c(1, 4, 128, 0.1)
referenceLink <- c(1, 1, 1, 2)
referenceNE <- c(1, 6, 1, 2)

#set.seed(1)
SP <- createOneSP(SPConfig[1], SPConfig[2], SPConfig[3])
  
SPhosts <- decomposeSP(SP, "hosts")
SPlinks <- decomposeSP(SP, "links")
SPnes <- decomposeSP(SP, "nes")
  
for ( i in 1:20 ) {
  
  numberOfProviders <- i
  results <- vector()
    
  for ( j in 1:numberOfTurns) {
    
    print(paste("provider", i, "turn", j, sep = " "))
      
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

resultsList <- ls(pattern = "results_")
rm(list = setdiff(ls(), resultsList))

results <- data.frame(matrix(nrow=1000,ncol=0))
for (i in 1:20) { auxDF <- get( paste("results_", i, sep="") ) ; results <- cbind(results,auxDF) ; rm(auxDF) }
rm(i)
colnames(results) <- 1:20

save.image()