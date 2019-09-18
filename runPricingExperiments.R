rm(list = ls())
source('./sourceAll.R')
SPConfig<-c(2,2,1)
numberOfProviders <- 5
numberOfTurns <- 2
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

for ( i in 1:numberOfTurns) {
  
  valid <- FALSE
  seed <- i
  
  while(!valid)  {

    set.seed(seed)
    
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
      seed <- (seed + sample(1:1000,1))
      source('./desourceAll.R')
    } else {
      valid <- TRUE
    }
    
  }

  auxHosts <- paste("hostsDF_", i, sep = "")
  assign(auxHosts, hostsDF, envir = .GlobalEnv)
  rm(auxHosts, hostsDF)  
  auxLinks <- paste("linksDF_", i, sep = "")
  assign(auxLinks, linksDF, envir = .GlobalEnv)
  rm(auxLinks, linksDF)  
  auxNEs <- paste("nesDF_", i, sep = "")
  assign(auxNEs, nesDF, envir = .GlobalEnv)
  rm(auxNEs, nesDF)
  
}

resultsList <- ls(pattern = "DF")
rm(list = setdiff(ls(), resultsList))
save.image()