rm(list = ls())
print(paste("#time start", Sys.time(), sep = " "))
source('./src/scripts/sourceAll.R')
SPConfig<-c(4,4,2)
minNumberOfProviders <- 5
maxNumberOfProviders <- 6
numberOfTurns <- 30
priceHostPerDay <- 100000
priceLinkPerDay <- 100000
priceNEPerDay <- 100000
pricingType <- "fixed"

referenceHost <- c(1, 4, 128, 0.1)
referenceLink <- c(1, 1, 1, 2)
referenceNE <- c(1, 6, 1, 2)

#set.seed()
SP <- createOneSP(SPConfig[1], SPConfig[2], SPConfig[3])
  
SPhosts <- decomposeSP(SP, "hosts")
minHosts <- data.frame(min(SPhosts$cpu), min(SPhosts$mem), min(SPhosts$str))
colnames(minHosts) <- c("cpu", "mem", "str")
SPlinks <- decomposeSP(SP, "links")
minLinks <- data.frame(min(SPlinks$cap), min(SPlinks$del), min(SPlinks$jit))
colnames(minLinks) <- c("cap", "del", "jit")
SPnes <- decomposeSP(SP, "nes")
minNEs <- data.frame(min(SPnes$cap), min(SPnes$por), min(SPnes$que))
colnames(minNEs) <- c("cap", "por", "que")

for ( i in minNumberOfProviders:maxNumberOfProviders ) {
  
  numberOfProviders <- i
  resultsVec <- vector()
    
  for ( j in 1:numberOfTurns) {
      
    valid <- FALSE
    count <- 1
    
    while(!valid)  {
      
      P <- createProviders(numberOfProviders, SPConfig[1], SPConfig[2], SPConfig[3])
        
      Phosts <- decomposeProv(P, "hosts", minHosts)
      Plinks <- decomposeProv(P, "links", minLinks)
      Pnes <- decomposeProv(P, "nes", minNEs)
      
      hostsDF <- data.frame()
      linksDF <- data.frame()
      nesDF <- data.frame()
      
      if ( nrow(SPhosts) < nrow(Phosts) ) { 
        hostsComb <- indexes(nrow(SPhosts), nrow(Phosts)) 
        hostsDF <- hostsAnswers(hostsComb, priceHostPerDay, pricingType)
      }
      if ( nrow(SPlinks) < nrow(Plinks) ) { 
        linksComb <- indexes(nrow(SPlinks), nrow(Plinks)) 
        linksDF <- linksAnswers(linksComb, priceLinkPerDay, pricingType)
      }
      if ( nrow(SPnes) < nrow(Pnes) ) { 
        nesComb <- indexes(nrow(SPnes), nrow(Pnes))
        nesDF <- nesAnswers(nesComb, priceNEPerDay, pricingType)
      }
      
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
      
    resultsVec[j] <- count
    
    print(paste("#providers", i, "turn", j, "time", Sys.time(), sep = " "))
      
  }
    
  auxName <- paste("results_", numberOfProviders, sep = "")
  assign(auxName, resultsVec, envir = .GlobalEnv)
  rm(auxName, resultsVec)
  save.image()
    
}

results_all <- data.frame(matrix(nrow=numberOfTurns,ncol=0))
for (i in minNumberOfProviders:maxNumberOfProviders) { auxDF <- get( paste("results_", i, sep="") ) ; results_all <- cbind(results_all,auxDF) ; rm(auxDF) }
rm(i)
colnames(results_all) <- minNumberOfProviders:maxNumberOfProviders


resultsList <- ls(pattern = "results_")
rm(list = setdiff(ls(), resultsList))

print(paste("#time end", Sys.time(), sep = " "))
save.image()