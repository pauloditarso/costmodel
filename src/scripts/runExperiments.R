print(paste("#time start", Sys.time(), sep = " "))

rm(list = ls())
if ( !("ggplot2" %in% (.packages())) )  { library(ggplot2) }
source('./src/scripts/sourceAll.R')

input <- read.delim("input.txt", header = F, sep = " ")
SPConfig <- c(input$V1, input$V1, (input$V1/2))
demand = switch (as.character(SPConfig[1]),
  "2"  = c("a) 2,2,1"),
  "4"  = c("b) 4,4,2"),
  "8"  = c("c) 8,8,4"),
  "16" = c("d) 16,16,8"),
  "32" = c("e) 32,32,16"),
  "64" = c("f) 64,64,32")
)

minNumberOfProviders <- 5
maxNumberOfProviders <- 20
numberOfTurns <- input$V2
priceHostPerDay <- 0
priceLinkPerDay <- 0
priceNEPerDay <- 0
increasePriceFactor <- 1.5
pricingType <- "fixed"
quota <- -1
referenceHost <- c(1, 4, 128, 0.1)
referenceLink <- c(1, 1, 1, 2)
referenceNE <- c(1, 6, 1, 2)
hostsLabels <- c("cpu", "mem", "str")
linksLabels <- c("cap", "del", "jit")
nesLabels <- c("cap", "por", "que")

# set.seed()

# starting block of choosing input demand #
satisfied <- FALSE
numberOfTrials <- 0
while(!satisfied) {
  
  numberOfTrials <- numberOfTrials + 1
  if (numberOfTrials == 1000) { stop("ERROR: infinite loop!!!") }
  
  SP <- createOneSP(SPConfig[1], SPConfig[2], SPConfig[3])
  
  if( length(SP$hosts) == SPConfig[1] & length(SP$links) == SPConfig[2] & 
      length(SP$nes) == SPConfig[3] ) {
    satisfied <- TRUE
  } 
  
}
# ending block of choosing input demand #

# starting block of decomposing input demand #
SPhosts <- decomposeSP(SP, "hosts")
SPhosts <- SPhosts[order(SPhosts$cost),]
rownames(SPhosts) <- 1:(nrow(SPhosts))
minHosts <- data.frame(min(SPhosts$cpu), min(SPhosts$mem), min(SPhosts$str))
colnames(minHosts) <- hostsLabels
SPhosts$cost <- round(SPhosts$cost, 2)

SPlinks <- decomposeSP(SP, "links")
SPlinks <- SPlinks[order(SPlinks$cost),]
rownames(SPlinks) <- 1:(nrow(SPlinks))
minLinks <- data.frame(min(SPlinks$cap), min(SPlinks$del), min(SPlinks$jit))
colnames(minLinks) <- linksLabels
SPlinks$cost <- round(SPlinks$cost, 2)

SPnes <- decomposeSP(SP, "nes")
SPnes <- SPnes[order(SPnes$cost),]
rownames(SPnes) <- 1:(nrow(SPnes))
minNEs <- data.frame(min(SPnes$cap), min(SPnes$por), min(SPnes$que))
colnames(minNEs) <- nesLabels
SPnes$cost <- round(SPnes$cost, 2)
# ending block of decomposing input demand #

# starting block of setting the price constraints per resources #
for ( m in 1:nrow(SPhosts) ) {
  auxPricing <- (24 * pricing(as.numeric(SPhosts[m,][1]), 
                              as.numeric(SPhosts[m,][2]), 
                              as.numeric(SPhosts[m,][3]), "hosts"))
  priceHostPerDay <- round( (priceHostPerDay + auxPricing), digits = 2 )
}
priceHostPerDay <- round( (priceHostPerDay * increasePriceFactor), digits = 2 )
rm(m, auxPricing)

for ( l in 1:nrow(SPlinks) ) {
  auxPricing <- (24 * pricing(as.numeric(SPlinks[l,][1]), 
                              as.numeric(SPlinks[l,][2]), 
                              as.numeric(SPlinks[l,][3]), 
                              "links"))
  priceLinkPerDay <- round( (priceLinkPerDay + auxPricing), digits = 2 )
}
priceLinkPerDay <- round( (priceLinkPerDay * increasePriceFactor), digits = 2 )
rm(l, auxPricing)

for ( n in 1:nrow(SPnes) ) {
  auxPricing <- (24 * pricing(as.numeric(SPnes[n,][1]), 
                              as.numeric(SPnes[n,][2]), 
                              as.numeric(SPnes[n,][3]), 
                              "nes"))
  priceNEPerDay <- round( (priceNEPerDay + auxPricing), digits = 2 )
}
priceNEPerDay <- round( (priceNEPerDay * increasePriceFactor), digits = 2 )
rm(n, auxPricing)
# ending block of setting the price constraints per resources #

finalCosts <- data.frame(matrix(ncol=6, nrow=0))
finalDisc <- data.frame(matrix(ncol=6, nrow=0))
finalRepsHosts <- data.frame(matrix(ncol=6, nrow=0))
finalRepsLinks <- data.frame(matrix(ncol=6, nrow=0))
finalRepsNEs <- data.frame(matrix(ncol=6, nrow=0))

for (numberOfProviders in minNumberOfProviders:maxNumberOfProviders) {

  turn <- 1
  print(paste("#providers", numberOfProviders, "of", maxNumberOfProviders, 
              "time", Sys.time(), sep = " "))
  
  while (turn <= numberOfTurns) {
    
    # starting block of setting providers confs #
    satisfied <- FALSE
    numberOfTrials <- 0
    while (!satisfied) {
      
      numberOfTrials <- numberOfTrials + 1
      if (numberOfTrials == 1000) { stop("ERROR: infinite loop!!!") }
      
      P <- createProviders(numberOfProviders, SPConfig[1], SPConfig[2], 
                           SPConfig[3], minHosts, minLinks, minNEs)
      
      Phosts <- decomposeProv(P, "hosts", minHosts)
      Phosts$price <- round(Phosts$price, digits = 2)
      Plinks <- decomposeProv(P, "links", minLinks)
      Plinks$price <- round(Plinks$price, digits = 2)
      Pnes <- decomposeProv(P, "nes", minNEs)
      Pnes$price <- round(Pnes$price, digits = 2)

      # verifying whether exists enough Host, Link and NE responses
      if ( nrow(Phosts) >= nrow(SPhosts) & nrow(Plinks) >= nrow(SPlinks) & 
           nrow(Pnes) >= nrow(SPnes) ) {
        
        # Hosts
        totalHostsOptions <- vector()
        numberOfOptions <- 0
        for ( i in 1:nrow(SPhosts) ) {
          
          numberOfOptions <- nrow(Phosts[Phosts$cpu >= SPhosts[i,]$cpu & 
              Phosts$mem >= SPhosts[i,]$mem & Phosts$str >= SPhosts[i,]$str,])
          
          totalHostsOptions <- c(totalHostsOptions, numberOfOptions)
          
        }
        
        # Links
        totalLinksOptions <- vector()
        numberOfOptions <- 0
        for ( i in 1:nrow(SPlinks) ) {
          
          numberOfOptions <- nrow(Plinks[Plinks$cap >= SPlinks[i,]$cap & 
              Plinks$del >= SPlinks[i,]$del & Plinks$jit >= SPlinks[i,]$jit,])
          
          totalLinksOptions <- c(totalLinksOptions, numberOfOptions)
          
        }
        
        # NEs
        totalNEsOptions <- vector()
        numberOfOptions <- 0
        for ( i in 1:nrow(SPnes) ) {
          
          numberOfOptions <- nrow(Pnes[Pnes$cap >= SPnes[i,]$cap & 
              Pnes$por >= SPnes[i,]$por & Pnes$que >= SPnes[i,]$que,])
          
          totalNEsOptions <- c(totalNEsOptions, numberOfOptions)
          
        }

      }
      
      if ( all(totalHostsOptions >= SPConfig[1]) & 
           all(totalLinksOptions >= SPConfig[2]) & 
           all(totalNEsOptions >= SPConfig[3]) ) 
        satisfied <- TRUE
      
    }
    # ending block of setting providers confs #
    
    scenarioNameHosts <- paste("hosts", "-", SPConfig[1], "-", SPConfig[2], "-", 
                         SPConfig[3], "-", numberOfProviders, sep = "")
    scenarioNameLinks <- paste("links", "-", SPConfig[1], "-", SPConfig[2], "-", 
                         SPConfig[3], "-", numberOfProviders, sep = "")
    scenarioNameNEs <- paste("nes", "-", SPConfig[1], "-", SPConfig[2], "-", 
                       SPConfig[3], "-", numberOfProviders, sep = "")
    
    if ( length(unique(Phosts$providerID)) < numberOfProviders |
         length(unique(Plinks$providerID)) < numberOfProviders |
         length(unique(Pnes$providerID)) < numberOfProviders ) {
      rm(list = ls())
      stop("ERROR: less providers than original amount!!!")
    }

    # starting block for hosts, links and NEs #
    
    # starting block for a random strategy #
    
    PhostsAux <- Phosts
    PlinksAux <- Plinks
    PnesAux <- Pnes
    randomOfferHosts <- data.frame(matrix(nrow = 0, ncol = 3))
    randomOfferLinks <- data.frame(matrix(nrow = 0, ncol = 3))
    randomOfferNEs <- data.frame(matrix(nrow = 0, ncol = 3))
    foundRandomOffer <- FALSE
    
    while (!foundRandomOffer) {
      
      # Hosts
      for ( demandID in nrow(SPhosts):1 ) {
        
        numberOfTrials <- 0
        satisfied <- FALSE
        while (!satisfied) {
          
          numberOfTrials <- numberOfTrials + 1
          # testing whether number of trials is higher than the offer
          if ( numberOfTrials > nrow(Phosts) ) {
            randomOfferHosts <- data.frame(matrix(nrow = 0, ncol = 3))
            PhostsAux <- Phosts
            break 
          }
          
          setOfChoices <- as.numeric(rownames(PhostsAux[PhostsAux$cpu >= 
                          SPhosts[demandID,]$cpu & PhostsAux$mem >= 
                          SPhosts[demandID,]$mem & PhostsAux$str >= 
                          SPhosts[demandID,]$str,]))
          
          if ( length(setOfChoices) == 1 ) {
            randomRow <- which(rownames(PhostsAux) == setOfChoices)
          } else if (length(setOfChoices) > 1) {
            randomAux <- sample(setOfChoices, 1)
            randomRow <- which(rownames(PhostsAux) == randomAux)
          } else {
            print("ERROR: empty set of choices (Hosts)!!!")
            stop()
          }
          
          randomSample <- PhostsAux[randomRow, hostsLabels]
          
          if ( all(SPhosts[demandID, hostsLabels] <= 
                   randomSample) ) {
            
            satisfied <- TRUE
            randomOfferHosts <- rbind( randomOfferHosts, c(demandID, 
                                        PhostsAux[randomRow,]$providerID, 
                                        PhostsAux[randomRow,]$resourceID) )
            PhostsAux <- PhostsAux[-randomRow,]
          
          }
        
        }
        
      } # end Hosts
      
      # Links
      for ( demandID in nrow(SPlinks):1 ) {
        
        numberOfTrials <- 0
        satisfied <- FALSE
        while (!satisfied) {
          
          numberOfTrials <- numberOfTrials + 1
          # testing whether number of trials is higher than the offer
          if ( numberOfTrials > nrow(Plinks) ) {
            randomOfferLinks <- data.frame(matrix(nrow = 0, ncol = 3))
            PlinksAux <- Plinks
            break 
          }
          
          setOfChoices <- as.numeric(rownames(PlinksAux[PlinksAux$cap >= 
                          SPlinks[demandID,]$cap & PlinksAux$del >= 
                          SPlinks[demandID,]$del & PlinksAux$jit >= 
                          SPlinks[demandID,]$jit,]))
          
          if ( length(setOfChoices) == 1 ) {
            randomRow <- which(rownames(PlinksAux) == setOfChoices)
          } else if (length(setOfChoices) > 1) {
            randomAux <- sample(setOfChoices, 1)
            randomRow <- which(rownames(PlinksAux) == randomAux)
          } else {
            print("ERROR: empty set of choices (Links)!!!")
            stop()
          }
          
          randomSample <- PlinksAux[randomRow, linksLabels]
          
          if ( all(SPlinks[demandID, linksLabels] <= 
                   randomSample) ) {
            
            satisfied <- TRUE
            randomOfferLinks <- rbind( randomOfferLinks, c(demandID, 
                                        PlinksAux[randomRow,]$providerID, 
                                        PlinksAux[randomRow,]$resourceID) )
            PlinksAux <- PlinksAux[-randomRow,]
            
          }
          
        }
        
      } # end Links
      
      # NEs
      for ( demandID in nrow(SPnes):1 ) {
        
        numberOfTrials <- 0
        satisfied <- FALSE
        while (!satisfied) {
          
          numberOfTrials <- numberOfTrials + 1
          # testing whether number of trials is higher than the offer
          if ( numberOfTrials > nrow(Pnes) ) {
            randomOfferNEs <- data.frame(matrix(nrow = 0, ncol = 3))
            PnesAux <- Pnes
            break 
          }
          
          setOfChoices <- as.numeric(rownames(PnesAux[PnesAux$cap >= 
                          SPnes[demandID,]$cap & PnesAux$por >= 
                          SPnes[demandID,]$por & PnesAux$que >= 
                          SPnes[demandID,]$que,]))
          
          if ( length(setOfChoices) == 1 ) {
            randomRow <- which(rownames(PnesAux) == setOfChoices)
          } else if (length(setOfChoices) > 1) {
            randomAux <- sample(setOfChoices, 1)
            randomRow <- which(rownames(PnesAux) == randomAux)
          } else {
            print("ERROR: empty set of choices (NEs)!!!")
            stop()
          }
          
          randomSample <- PnesAux[randomRow, nesLabels]
          
          if ( all(SPnes[demandID, nesLabels] <= 
                   randomSample) ) {
            
            satisfied <- TRUE
            randomOfferNEs <- rbind( randomOfferNEs, c(demandID, 
                                        PnesAux[randomRow,]$providerID, 
                                        PnesAux[randomRow,]$resourceID) )
            PnesAux <- PnesAux[-randomRow,]
            
          }
          
        }
        
      } # end NEs
      
      if ( nrow(randomOfferHosts) == nrow(SPhosts) &
           nrow(randomOfferLinks) == nrow(SPlinks) &
           nrow(randomOfferNEs) == nrow(SPnes) ) {
        foundRandomOffer <- TRUE
      }
      
    }
    
    colnames(randomOfferHosts) <- c("demandID", "providerID", "resourceID")
    colnames(randomOfferLinks) <- c("demandID", "providerID", "resourceID")
    colnames(randomOfferNEs) <- c("demandID", "providerID", "resourceID")
    
    randomCost <- 0; 
    auxProvID <- 0; auxResID <- 0
    for ( i in 1:nrow(randomOfferHosts) ) {
      auxProvID <- randomOfferHosts[i,]$providerID
      auxResID <- randomOfferHosts[i,]$resourceID
      randomCost <- randomCost + (Phosts[Phosts$providerID == auxProvID & 
                                  Phosts$resourceID == auxResID,]$price) 
    }
    auxProvID <- 0; auxResID <- 0
    for ( i in 1:nrow(randomOfferLinks) ) {
      auxProvID <- randomOfferLinks[i,]$providerID
      auxResID <- randomOfferLinks[i,]$resourceID
      randomCost <- randomCost + (Plinks[Plinks$providerID == auxProvID & 
                                  Plinks$resourceID == auxResID,]$price) 
    }
    auxProvID <- 0; auxResID <- 0
    for ( i in 1:nrow(randomOfferNEs) ) {
      auxProvID <- randomOfferNEs[i,]$providerID
      auxResID <- randomOfferNEs[i,]$resourceID
      randomCost <- randomCost + (Pnes[Pnes$providerID == auxProvID & 
                                  Pnes$resourceID == auxResID,]$price) 
    }
    
    rm(PhostsAux, PlinksAux, PnesAux, auxProvID, auxResID,
       randomOfferHosts, randomOfferLinks, randomOfferNEs)
    
    # ending block for a random strategy #
    
    #starting block for first and optimized strategies #
    
    fHosts <- file(paste("./files/", scenarioNameHosts, ".txt", sep = "" ), "w")
    fLinks <- file(paste("./files/", scenarioNameLinks, ".txt", sep = "" ), "w")
    fNEs <- file(paste("./files/", scenarioNameNEs, ".txt", sep = "" ), "w")
    
    writeLines( noquote(paste(length(unique(Phosts$providerID)), 
                nrow(SPhosts), sep = " ")), con = fHosts, sep = "\n" )
    writeLines( noquote(paste(length(unique(Plinks$providerID)), 
                nrow(SPlinks), sep = " ")), con = fLinks, sep = "\n" )
    writeLines( noquote(paste(length(unique(Pnes$providerID)), 
                nrow(SPnes), sep = " ")), con = fNEs, sep = "\n" )
  
    firstOfferHosts <- data.frame(matrix(nrow = 0, ncol = 3))
    firstOfferLinks <- data.frame(matrix(nrow = 0, ncol = 3))
    firstOfferNEs <- data.frame(matrix(nrow = 0, ncol = 3))
    
    foundFirstOfferHosts <- FALSE
    foundFirstOfferLinks <- FALSE
    foundFirstOfferNEs <- FALSE
    
    lastDemandHosts <- 0
    lastResourceHosts <- 0
    lastProviderHosts <- 0
    
    lastDemandLinks <- 0
    lastResourceLinks <- 0
    lastProviderLinks <- 0
    
    lastDemandNEs <- 0
    lastResourceNEs <- 0
    lastProviderNEs <- 0
    
    # Hosts
    for ( providerID in unique(Phosts$providerID) ) {
      
      finalStrHosts <- "{"
      
      for (resourceID in Phosts[Phosts$providerID == providerID, ]$resourceID) {
        
        auxProvResource <- Phosts[Phosts$providerID == providerID & 
              Phosts$resourceID == resourceID, c("cpu", "mem", "str", "price")]
        auxCond <- FALSE
        auxStr <- ""
        
        for ( demandID in 1:nrow(SPhosts) ) {
          
          if ( all( auxProvResource[hostsLabels] >= 
                    SPhosts[demandID, hostsLabels] ) ) {

            if ( nrow(firstOfferHosts) == nrow(SPhosts) ) {
              foundFirstOfferHosts <- TRUE
            }            
            if ( (!foundFirstOfferHosts) & (lastDemandHosts < demandID) ) {
              
              if (lastProviderHosts != providerID | 
                  lastResourceHosts != resourceID) {
                firstOfferHosts <- rbind( firstOfferHosts, 
                                          c(providerID, resourceID, demandID) )
                lastDemandHosts <- demandID
                lastResourceHosts <- resourceID
                lastProviderHosts <- providerID
              }
              
            }

            if ( auxCond == FALSE ) {
              auxStr <- paste(demandID-1)
            }
            else {
              auxStr <- paste(auxStr, ",", (demandID-1), sep = "")  
            }
            auxCond <- TRUE
          
          }
          
        }
        
        colnames(firstOfferHosts) <- c("providerID", "resourceID", "demandID")
        
        if ( auxCond == TRUE ) { finalStrHosts <- paste(finalStrHosts, 
                          auxStr, ":", auxProvResource["price"], sep = " ") }
        
        if ( resourceID != 
             tail(Phosts[Phosts$providerID == providerID, ]$resourceID, n=1)) {
          finalStrHosts <- paste(finalStrHosts, ";", sep = " ")
        }
        else {
          finalStrHosts <- paste(finalStrHosts, "}", sep = " ") 
        }
        
      }
      
      writeLines( noquote(finalStrHosts), con = fHosts, sep = "\n" )
    
    } # end Hosts
    
    # Links
    for ( providerID in unique(Plinks$providerID) ) {
      
      finalStrLinks <- "{"
      
      for (resourceID in Plinks[Plinks$providerID == providerID, ]$resourceID) {
        
        auxProvResource <- Plinks[Plinks$providerID == providerID & 
              Plinks$resourceID == resourceID, c("cap", "del", "jit", "price")]
        auxCond <- FALSE
        auxStr <- ""
        
        for ( demandID in 1:nrow(SPlinks) ) {
          
          if ( all( auxProvResource[linksLabels] >= 
                    SPlinks[demandID, linksLabels] ) ) {
            
            if ( nrow(firstOfferLinks) == nrow(SPlinks) ) {
              foundFirstOfferLinks <- TRUE
            }            
            if ( (!foundFirstOfferLinks) & (lastDemandLinks < demandID) ) {
              
              if (lastProviderLinks != providerID | 
                  lastResourceLinks != resourceID) {
                firstOfferLinks <- rbind( firstOfferLinks, 
                                          c(providerID, resourceID, demandID) )
                lastDemandLinks <- demandID
                lastResourceLinks <- resourceID
                lastProviderLinks <- providerID
              }
              
            }
            
            if ( auxCond == FALSE ) {
              auxStr <- paste(demandID-1)
            }
            else {
              auxStr <- paste(auxStr, ",", (demandID-1), sep = "")  
            }
            auxCond <- TRUE
            
          }
          
        }
        
        colnames(firstOfferLinks) <- c("providerID", "resourceID", "demandID")
        
        if ( auxCond == TRUE ) { 
          finalStrLinks <- paste(finalStrLinks, auxStr, ":", 
                            auxProvResource["price"], sep = " ") }
        
        if ( resourceID != 
             tail(Plinks[Plinks$providerID == providerID, ]$resourceID, n=1)) {
          finalStrLinks <- paste(finalStrLinks, ";", sep = " ")
        }
        else {
          finalStrLinks <- paste(finalStrLinks, "}", sep = " ") 
        }
        
      }
      
      writeLines( noquote(finalStrLinks), con = fLinks, sep = "\n" )
      
    } # end Links
    
    # NEs
    for ( providerID in unique(Pnes$providerID) ) {
      
      finalStrNEs <- "{"
      
      for (resourceID in Pnes[Pnes$providerID == providerID, ]$resourceID) {
        
        auxProvResource <- Pnes[Pnes$providerID == providerID & 
                Pnes$resourceID == resourceID, c("cap", "por", "que", "price")]
        auxCond <- FALSE
        auxStr <- ""
        
        for ( demandID in 1:nrow(SPnes) ) {
          
          if ( all( auxProvResource[nesLabels] >= 
                    SPnes[demandID, nesLabels] ) ) {
            
            if ( nrow(firstOfferNEs) == nrow(SPnes) ) {
              foundFirstOfferNEs <- TRUE
            }            
            if ( (!foundFirstOfferNEs) & (lastDemandNEs < demandID) ) {
              
              if (lastProviderNEs != providerID | 
                  lastResourceNEs != resourceID) {
                firstOfferNEs <- rbind( firstOfferNEs, 
                                        c(providerID, resourceID, demandID) )
                lastDemandNEs <- demandID
                lastResourceNEs <- resourceID
                lastProviderNEs <- providerID
              }
              
            }
            
            if ( auxCond == FALSE ) {
              auxStr <- paste(demandID-1)
            }
            else {
              auxStr <- paste(auxStr, ",", (demandID-1), sep = "")  
            }
            auxCond <- TRUE
            
          }
          
        }
        
        colnames(firstOfferNEs) <- c("providerID", "resourceID", "demandID")
        
        if ( auxCond == TRUE ) { 
          finalStrNEs <- paste(finalStrNEs, auxStr, ":", 
                                 auxProvResource["price"], sep = " ") }
        
        if ( resourceID != 
             tail(Pnes[Pnes$providerID == providerID, ]$resourceID, n=1)) {
          finalStrNEs <- paste(finalStrNEs, ";", sep = " ")
        }
        else {
          finalStrNEs <- paste(finalStrNEs, "}", sep = " ") 
        }
        
      }
      
      writeLines( noquote(finalStrNEs), con = fNEs, sep = "\n" )
      
    } # end NEs
    
    close(fHosts)
    close(fLinks)
    close(fNEs)
    rm(fHosts, fLinks, fNEs, providerID, resourceID, demandID, auxCond, auxStr, 
       finalStrHosts, finalStrLinks, finalStrNEs, auxProvResource)
    
    bashCommandHosts <- paste("bash files/count.sh files/", 
                         scenarioNameHosts, ".txt", sep = "")
    testBashHosts <- as.numeric(system(bashCommandHosts, intern = TRUE))
    testProvHosts <- tabulate(Phosts$providerID)
    if (any(testBashHosts != testProvHosts)) { 
      print("ERROR: Hosts tests are different!!!") 
    }
    rm(bashCommandHosts, testBashHosts, testProvHosts)
    
    bashCommandLinks <- paste("bash files/count.sh files/", 
                         scenarioNameLinks, ".txt", sep = "")
    testBashLinks <- as.numeric(system(bashCommandLinks, intern = TRUE))
    testProvLinks <- tabulate(Plinks$providerID)
    if (any(testBashLinks != testProvLinks)) { 
      print("ERROR: Links tests are different!!!") 
    }
    rm(bashCommandLinks, testBashLinks, testProvLinks)
    
    bashCommandNEs <- paste("bash files/count.sh files/", 
                         scenarioNameNEs, ".txt", sep = "")
    testBashNEs <- as.numeric(system(bashCommandNEs, intern = TRUE))
    testProvNEs <- tabulate(Pnes$providerID)
    if (any(testBashNEs != testProvNEs)) { 
      print("ERROR: NEs tests are different!!!") 
    }
    rm(bashCommandNEs, testBashNEs, testProvNEs)
    
    solverCommandHosts <- paste("bash solver/gera.sh files/", 
                           scenarioNameHosts, ".txt", sep = "" )
    logSolverHosts <- system(solverCommandHosts, intern = TRUE)
    
    solverCommandLinks <- paste("bash solver/gera.sh files/", 
                           scenarioNameLinks, ".txt", sep = "" )
    logSolverLinks <- system(solverCommandLinks, intern = TRUE)
    
    solverCommandNEs <- paste("bash solver/gera.sh files/", 
                           scenarioNameNEs, ".txt", sep = "" )
    logSolverNEs <- system(solverCommandNEs, intern = TRUE)
    
    if ( file.info(paste("files/", scenarioNameHosts, ".opt", sep = ""))$size 
         != 0 & nrow(firstOfferHosts) == nrow(SPhosts) &
         file.info(paste("files/", scenarioNameLinks, ".opt", sep = ""))$size 
         != 0 & nrow(firstOfferLinks) == nrow(SPlinks) &
         file.info(paste("files/", scenarioNameNEs, ".opt", sep = ""))$size 
         != 0 & nrow(firstOfferNEs) == nrow(SPnes)
       ) 
    {
      
      responseOptHosts <- read.csv(paste("files/", scenarioNameHosts, ".opt", 
                                    sep = ""), header = F)
      finalRepsHosts <- rbind( finalRepsHosts, c(demand, SPConfig[1], 
              numberOfProviders, turn, length(unique(responseOptHosts$V1)), 1) )

      responseOptLinks <- read.csv(paste("files/", scenarioNameLinks, ".opt", 
                                    sep = ""), header = F)
      finalRepsLinks <- rbind( finalRepsLinks, c(demand, SPConfig[2], 
              numberOfProviders, turn, length(unique(responseOptLinks$V1)), 1) )

      responseOptNEs <- read.csv(paste("files/", scenarioNameNEs, ".opt", 
                                    sep = ""), header = F)
      finalRepsNEs <- rbind( finalRepsNEs, c(demand, SPConfig[3], 
              numberOfProviders, turn, length(unique(responseOptNEs$V1)), 1) )
      
      #### parei aqui hoje 26/05/22

      # starting block of optimized cost
      optCost <- 0
      providerID <- 0
      resourceID <- 0
      for (i in 1:nrow(responseOptHosts)) { 
        providerID <- responseOptHosts[i,]$V1
        resourceID <- responseOptHosts[i,]$V2
        optCost <- optCost + 
          as.numeric(Phosts[Phosts$providerID == providerID & 
                              Phosts$resourceID == resourceID,]$price)
      }
      # ending block of optimized cost
      
      # starting block of first choice cost #
      firstCost <- 0
      auxProvID <- 0
      auxResID <- 0
      for ( i in 1:nrow(firstOfferHosts) ) {
        auxProvID <- firstOfferHosts[i,]$providerID
        auxResID <- firstOfferHosts[i,]$resourceID
        firstCost <- firstCost + (Phosts[Phosts$providerID == auxProvID & 
                                        Phosts$resourceID == auxResID,]$price) 
      }
      # ending block of first choice cost #
      
      if ( optCost > firstCost ) { 
        print("ERROR: optimized cost greater than first choice!!!") 
      }
      
      # starting block of discounted cost #
      discountRates <- c(0.1, 0.2, 0.3)
      for ( discountRate in discountRates ) {

        targetCost <- optCost
        responseDis <- responseOptHosts
        usedOptProvs <- unique(responseOptHosts$V1)
        
        for ( usedOptProv in usedOptProvs ) {
          
          auxDis <- data.frame(matrix(ncol = 2, nrow = SPConfig[1]))
          usedLines <- which(responseOptHosts$V1 == usedOptProv)
          notUsedLines <- which(!responseOptHosts$V1 == usedOptProv)
          auxDis[usedLines,] <- responseOptHosts[usedLines,]
          usedResources <- responseOptHosts[responseOptHosts$V1 == usedOptProv,]$V2
          notUsedResources <- which( !Phosts[Phosts$providerID == usedOptProv,
                                             ]$resourceID %in% usedResources )
          
          for ( demandLine in notUsedLines ) {
            
            lastUsedResource <- 0
            
            optPrice <- Phosts[Phosts$providerID == responseOptHosts[demandLine,]$V1 
                    & Phosts$resourceID == responseOptHosts[demandLine,]$V2, ]$price
            
            for ( notUsedResource in notUsedResources ) {
              
              if ( all(SPhosts[demandLine, hostsLabels] <= 
                   Phosts[Phosts$providerID == usedOptProv & 
                         Phosts$resourceID == notUsedResource, hostsLabels]) ) {
                
                auxPrice <- Phosts[Phosts$providerID == usedOptProv & 
                            Phosts$resourceID == notUsedResource, ]$price
                disPrice <- auxPrice - (auxPrice * discountRate)
                
                if ( disPrice <= optPrice ) {
                  
                  optPrice <- disPrice
                  lastUsedResource <- notUsedResource
                  auxDis[demandLine,]$X1 <- usedOptProv
                  auxDis[demandLine,]$X2 <- notUsedResource
                  
                }
                
              }
              
            }
            
            if ( lastUsedResource != 0 ) {
              notUsedResources <- notUsedResources[which(notUsedResources 
                                                         != lastUsedResource)]
            }
            
          }
          
          naLines <- which(is.na(auxDis$X1))
          if ( length(naLines) > 0 ) auxDis[naLines,] <- responseOptHosts[naLines,]
          
          auxDisCost <- 0
          disProvID <- 0
          disResID <- 0
          diffLines <- which(responseOptHosts$V1 != auxDis$X1)
          
          for (k in 1:nrow(auxDis)) {
            
            disProvID <- auxDis[k,]$X1
            disResID <- auxDis[k,]$X2
            tmpCost <- as.numeric(Phosts[Phosts$providerID == disProvID & 
                                         Phosts$resourceID == disResID,]$price)
            if ( k %in% diffLines ) {
              auxDisCost <- auxDisCost + (tmpCost - (tmpCost * discountRate))  
            }
            else {
              auxDisCost <- auxDisCost + tmpCost
            }
            
          }
          rm(k)
          
          if ( auxDisCost <= targetCost ) {
            responseDis <- auxDis
            targetCost <- auxDisCost
          }
          
          
        }
        
        if ( targetCost > optCost ) { 
          print("ERROR: target cost higher than opt cost!!!")
        }
        
        # if ( any(auxDis != responseOptHosts) ) {
        #   print("ERROR: auxDis is different from responseOptHosts!!!")
        # }
      
        if ( discountRate == 0.1 ) {
          finalRepsHosts <- rbind( finalRepsHosts, c(demand, SPConfig[1], 
                  numberOfProviders, turn, length(unique(responseDis$X1)), 4) )
          finalDisc <- rbind(finalDisc, c(demand, numberOfProviders, turn, 4,
                                        targetCost, optCost))
        }
        if ( discountRate == 0.2 ) {
          finalRepsHosts <- rbind( finalRepsHosts, c(demand, SPConfig[1], 
                  numberOfProviders, turn, length(unique(responseDis$X1)), 5) )  
          finalDisc <- rbind(finalDisc, c(demand, numberOfProviders, turn, 5,
                                        targetCost, optCost))
        }
        if ( discountRate == 0.3 ) {
          finalRepsHosts <- rbind( finalRepsHosts, c(demand, SPConfig[1], 
                  numberOfProviders, turn, length(unique(responseDis$X1)), 6) )  
          finalDisc <- rbind(finalDisc, c(demand, numberOfProviders, turn, 6,
                                        targetCost, optCost))
        }
        
      }
      # ending block of discounted cost #
      
      finalCosts <- rbind(finalCosts, c(demand, numberOfProviders, turn, 
                                          optCost, firstCost, randomCost))
      turn <- turn + 1
      
      # comment for debugging
      rm(optCost, providerID, resourceID, responseOptHosts, firstCost,
         auxResID, auxProvID, randomCost)
      
    }
    
    rm(solverCommandHosts, solverCommandLinks, solverCommandNEs, 
       logSolverHosts, logSolverLinks, logSolverNEs)
    # comment for debugging
    rm(numberOfTrials, P, Phosts, Plinks, Pnes, satisfied, foundFirstOfferHosts,
    firstOfferHosts, lastDemandHosts, lastResourceHosts, lastProviderHosts)
    
    #starting block for first and optimized strategies #
    
    # ending block for hosts #
    
  }
  
}

colnames(finalCosts) <- c("demand", "provs", "turn", "opt", "first", "random")
colnames(finalDisc) <- c("demand", "provs", "turn", "type", "target", "opt")
colnames(finalRepsHosts) <- c("demand", "hosts", "provs", 
                              "turn", "used", "type")
colnames(finalRepsLinks) <- c("demand", "hosts", "provs", 
                              "turn", "used", "type")
colnames(finalRepsNEs) <- c("demand", "hosts", "provs", 
                              "turn", "used", "type")

finalRepsHostsCI <- data.frame(matrix(ncol = 6, nrow = 0), 
                               stringsAsFactors = FALSE)
finalCostsCI <- data.frame(matrix(ncol = 6, nrow = 0), stringsAsFactors = FALSE)

for ( i in unique(finalCosts$provs) ) { 

  # '1' means optimized cost
  finalCostsCI <- rbind( finalCostsCI, c(demand, i, 1, 
        round(Rmisc::CI(as.numeric(finalCosts[finalCosts$provs == i,]$opt)), 
              digits = 2)) )
  # '2' means first cost
  finalCostsCI <- rbind( finalCostsCI, c(demand, i, 2, 
        round(Rmisc::CI(as.numeric(finalCosts[finalCosts$provs == i,]$first)),
              digits = 2)) )
  # '3' means random cost
  finalCostsCI <- rbind( finalCostsCI, c(demand, i, 3, 
        round(Rmisc::CI(as.numeric(finalCosts[finalCosts$provs == i,]$random)),
              digits = 2)) )
  
}
colnames(finalCostsCI) <- c("demand", "provs", "type", "upper", "mean", "lower")
rm(i)

repsTypes <- c(1, 4, 5, 6)
for ( i in unique(finalRepsHosts$provs) ) { 
  
  for ( repsType in repsTypes ) {

    used <- as.numeric(finalRepsHosts[finalRepsHosts$provs == i & 
                                 finalRepsHosts$type == repsType,]$used)
    hosts <- as.numeric(finalRepsHosts[finalRepsHosts$provs == i & 
                                  finalRepsHosts$type == repsType,]$hosts)
    percentage <- used/hosts
    finalRepsHostsCI <- rbind( finalRepsHostsCI, c(demand, i, repsType,
                          round(Rmisc::CI(percentage), digits = 2)) )
    
  }
  
}
colnames(finalRepsHostsCI) <- c("demand", "provs", "type", 
                                "upper", "mean", "lower")
rm(i, repsTypes)

# finalDiscCI <- data.frame(matrix(nrow = 0, ncol = 6))
# discTypes <- c(4, 5, 6)
# providers <- 5:20
# for ( provider in providers ) {
# 
#   for ( discType in discTypes ) {
#   
#     targetAux <- as.numeric(allDisc[allDisc$provs == provider & 
#                             allDisc$type == discType,]$target)
#     optAux <- as.numeric(allDisc[allDisc$provs == provider & 
#                          allDisc$type == discType,]$opt)
#     finalDiscCI <- rbind( finalDiscCI, 
#                           c(demand, 
#                             provider, 
#                             discType, 
#                             Rmisc::CI(targetAux/optAux)
#                            ) 
#                         )
#     
#   }
# 
# }

# costPlot <- ggplot( finalCostsCI, aes(x=provs, y=mean, ymin=lower, ymax=upper,
#                                     group=type, color = factor(-type)) ) +
#   theme_bw() +
#   geom_point() +
#   geom_smooth(stat="identity") +
#   theme(legend.position = "right", panel.grid.minor.x = element_blank()) +
#   xlab("Number of Providers") + ylab("Average slice cost") +
#   scale_x_continuous(breaks=seq(5, 20, 1)) +
#   scale_color_discrete("Legend:", breaks = unique(factor(-finalCostsCI$type)),
#                        labels = c("opt", "first", "random")) +
#   facet_wrap(~demand, scales = "free_y")
#ggsave(paste("h", "-", SPConfig[1], "-", SPConfig[2], "-", 
#SPConfig[3], ".pdf", sep = ""), plot = costPlot, device = "pdf")

# comment for debugging
rm(list=setdiff(ls(), ls(pattern = "final")))
save.image()

print(paste("#time end", Sys.time(), sep = " "))