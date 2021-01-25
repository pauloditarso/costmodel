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

#set.seed()

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
finalReps <- data.frame(matrix(ncol=6, nrow=0))
finalDisc <- data.frame(matrix(ncol=6, nrow=0))

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
      
      if ( nrow(Phosts) >= nrow(SPhosts) & nrow(Plinks) >= nrow(SPlinks) & 
           nrow(Pnes) >= nrow(SPnes) ) {
        
        totalOptions <- vector()
        for ( i in nrow(SPhosts) ) {
          
          numberOfOptions <- nrow(Phosts[Phosts$cpu >= SPhosts[i,]$cpu & 
              Phosts$mem >= SPhosts[i,]$mem & Phosts$str >= SPhosts[i,]$str,])
          
          totalOptions <- c(totalOptions, numberOfOptions)
          
        }
        
        if ( all(totalOptions != 0) ) satisfied <- TRUE
        
      }
      
    }
    # ending block of setting providers confs #
    
    scenarioName <- paste("h", "-", SPConfig[1], "-", SPConfig[2], "-", 
                          SPConfig[3], "-", numberOfProviders, sep = "")
    
    if ( length(unique(Phosts$providerID)) < numberOfProviders |
         length(unique(Plinks$providerID)) < numberOfProviders |
         length(unique(Pnes$providerID)) < numberOfProviders ) {
      rm(list = ls())
      stop("ERROR: less providers than original amount!!!")
    }

    # starting block for hosts #
    
    # starting block for a random strategy #
    
    PhostsAux <- Phosts
    randomOfferHosts <- data.frame(matrix(nrow = 0, ncol = 3))
    foundRandomOfferHosts <- FALSE
    
    while (!foundRandomOfferHosts) {
      
      for ( demandID in nrow(SPhosts):1 ) {
        
        numberOfTrials <- 0
        satisfied <- FALSE
        while (!satisfied) {
          
          numberOfTrials <- numberOfTrials + 1
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
          } else {
            randomAux <- sample(setOfChoices, 1)
            randomRow <- which(rownames(PhostsAux) == randomAux)
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
        
      }
      
      if ( nrow(randomOfferHosts) == nrow(SPhosts) ) {
        foundRandomOfferHosts <- TRUE
      }
    
    }
    
    colnames(randomOfferHosts) <- c("demandID", "providerID", "resourceID")
    
    randomCost <- 0
    auxProvID <- 0; auxResID <- 0
    for ( i in 1:nrow(randomOfferHosts) ) {
      auxProvID <- randomOfferHosts[i,]$providerID
      auxResID <- randomOfferHosts[i,]$resourceID
      randomCost <- randomCost + (Phosts[Phosts$providerID == auxProvID & 
                                         Phosts$resourceID == auxResID,]$price) 
    }
    
    rm(PhostsAux, randomOfferHosts, auxProvID, auxResID)
    
    # ending block for a random strategy #
    
    #starting block for first and optimized strategies #
    
    fHosts <- file(paste("./files/", scenarioName, ".txt", sep = "" ), "w")
    writeLines( noquote(paste(length(unique(Phosts$providerID)), nrow(SPhosts), 
                              sep = " ")), con = fHosts, sep = "\n" )
  
    firstOfferHosts <- data.frame(matrix(nrow = 0, ncol = 3))
    foundFirstOfferHosts <- FALSE
    lastDemandHosts <- 0
    lastResourceHosts <- 0
    lastProviderHosts <- 0
    
    for ( providerID in unique(Phosts$providerID) ) {
      
      finalStr <- "{"
      
      for (resourceID in Phosts[Phosts$providerID == providerID, ]$resourceID) {
        
        auxProvResource <- Phosts[Phosts$providerID == providerID & 
                                    Phosts$resourceID == resourceID, 
                                  c("cpu", "mem", "str", "price")]
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
        
        if ( auxCond == TRUE ) { finalStr <- paste(finalStr, auxStr, ":", 
                                                   auxProvResource["price"], 
                                                   sep = " ") }
        
        if ( resourceID != 
             tail(Phosts[Phosts$providerID == providerID, ]$resourceID, n=1)) {
          finalStr <- paste(finalStr, ";", sep = " ")
        }
        else {
          finalStr <- paste(finalStr, "}", sep = " ") 
        }
        
      }
      
      writeLines( noquote(finalStr), con = fHosts, sep = "\n" )
    }
    close(fHosts)
    rm(fHosts, providerID, resourceID, demandID, finalStr, 
       auxCond, auxStr, auxProvResource)
    
    bashCommand <- paste("bash files/count.sh files/", 
                         scenarioName, ".txt", sep = "")
    testBash <- as.numeric(system(bashCommand, intern = TRUE))
    testProv <- tabulate(Phosts$providerID)
    if (any(testBash != testProv)) { print("ERROR: tests are different!!!") }
    rm(bashCommand, testBash, testProv)
    
    solverCommand <- paste("bash solver/gera.sh files/", 
                           scenarioName, ".txt", sep = "" )
    logSolver <- system(solverCommand, intern = TRUE)
    
    if ( file.info(paste("files/", scenarioName, ".opt", sep = ""))$size != 0 & 
         nrow(firstOfferHosts) == nrow(SPhosts) ) {
      
      responseOpt <- read.csv(paste("files/", scenarioName, ".opt", sep = ""), 
                              header = F)
      
      finalReps <- rbind( finalReps, c(demand, SPConfig[1], numberOfProviders, 
                                turn, length(unique(responseOpt$V1)), 1) )
      
      # starting block of optimized cost
      optCost <- 0
      providerID <- 0
      resourceID <- 0
      for (i in 1:nrow(responseOpt)) { 
        providerID <- responseOpt[i,]$V1
        resourceID <- responseOpt[i,]$V2
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
        responseDis <- responseOpt
        usedOptProvs <- unique(responseOpt$V1)
        
        for ( usedOptProv in usedOptProvs ) {
          
          auxDis <- data.frame(matrix(ncol = 2, nrow = SPConfig[1]))
          usedLines <- which(responseOpt$V1 == usedOptProv)
          notUsedLines <- which(!responseOpt$V1 == usedOptProv)
          auxDis[usedLines,] <- responseOpt[usedLines,]
          usedResources <- responseOpt[responseOpt$V1 == usedOptProv,]$V2
          notUsedResources <- which( !Phosts[Phosts$providerID == usedOptProv,
                                             ]$resourceID %in% usedResources )
          
          for ( demandLine in notUsedLines ) {
            
            lastUsedResource <- 0
            
            optPrice <- Phosts[Phosts$providerID == responseOpt[demandLine,]$V1 
                    & Phosts$resourceID == responseOpt[demandLine,]$V2, ]$price
            
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
          if ( length(naLines) > 0 ) auxDis[naLines,] <- responseOpt[naLines,]
          
          auxDisCost <- 0
          disProvID <- 0
          disResID <- 0
          diffLines <- which(responseOpt$V1 != auxDis$X1)
          
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
        
        if ( targetCost > optCost ) print("ERROR: target cost higher than opt cost!!!")
        
        # if ( any(auxDis != responseOpt) ) {
        #   print("ERROR: auxDis is different from responseOpt!!!")
        # }
      
        if ( discountRate == 0.1 ) {
          finalReps <- rbind( finalReps, c(demand, SPConfig[1], 
                  numberOfProviders, turn, length(unique(responseDis$X1)), 4) )
          finalDisc <- rbind(finalDisc, c(demand, numberOfProviders, turn, 4,
                                        targetCost, optCost))
        }
        if ( discountRate == 0.2 ) {
          finalReps <- rbind( finalReps, c(demand, SPConfig[1], 
                  numberOfProviders, turn, length(unique(responseDis$X1)), 5) )  
          finalDisc <- rbind(finalDisc, c(demand, numberOfProviders, turn, 5,
                                        targetCost, optCost))
        }
        if ( discountRate == 0.3 ) {
          finalReps <- rbind( finalReps, c(demand, SPConfig[1], 
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
      rm(optCost, providerID, resourceID, responseOpt, firstCost,
         auxResID, auxProvID, randomCost)
      
    }
    rm(solverCommand, logSolver)
    # comment for debugging
    rm(numberOfTrials, P, Phosts, Plinks, Pnes, satisfied, foundFirstOfferHosts,
    firstOfferHosts, lastDemandHosts, lastResourceHosts, lastProviderHosts)
    
    #starting block for first and optimized strategies #
    
    # ending block for hosts #
    
  }
  
}

colnames(finalReps) <- c("demand", "hosts", "provs", "turn", "used", "type")
colnames(finalDisc) <- c("demand", "provs", "turn", "type", "target", "opt")
colnames(finalCosts) <- c("demand", "provs", "turn", "opt", "first", "random")

finalRepsCI <- data.frame(matrix(ncol = 6, nrow = 0), stringsAsFactors = FALSE)
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
for ( i in unique(finalReps$provs) ) { 
  
  for ( repsType in repsTypes ) {

    used <- as.numeric(finalReps[finalReps$provs == i & 
                                 finalReps$type == repsType,]$used)
    hosts <- as.numeric(finalReps[finalReps$provs == i & 
                                  finalReps$type == repsType,]$hosts)
    percentage <- used/hosts
    finalRepsCI <- rbind( finalRepsCI, c(demand, i, repsType,
                          round(Rmisc::CI(percentage), digits = 2)) )
    
  }
  
}
colnames(finalRepsCI) <- c("demand", "provs", "type", "upper", "mean", "lower")
rm(i, repsTypes)

# finalDiscCI <- data.frame(matrix(nrow = 0, ncol = 6))
# discTypes <- c(4, 5, 6)
# providers <- 5:20
# for ( provider in providers ) {
# 
#   for ( discType in discTypes ) {
#   
#     targetAux <- as.numeric(allDisc[allDisc$provs == provider & allDisc$type == discType,]$target)
#     optAux <- as.numeric(allDisc[allDisc$provs == provider & allDisc$type == discType,]$opt)
#     finalDiscCI <- rbind( finalDiscCI, c(demand, provider, discType, Rmisc::CI(targetAux/optAux)) )
#     
#   }
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