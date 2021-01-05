print(paste("#time start", Sys.time(), sep = " "))

rm(list = ls())
if ( !("ggplot2" %in% (.packages())) )  { library(ggplot2) }
source('./src/scripts/sourceAll.R')

SPConfig<-c(2,2,1)
#numberOfProviders <- 1
minNumberOfProviders <- 5
maxNumberOfProviders <- 20
numberOfTurns <- 100
priceHostPerDay <- 0
priceLinkPerDay <- 0
priceNEPerDay <- 0
increasePriceFactor <- 1.5
pricingType <- "fixed"
quota <- -1

referenceHost <- c(1, 4, 128, 0.1)
referenceLink <- c(1, 1, 1, 2)
referenceNE <- c(1, 6, 1, 2)

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
minHosts <- data.frame(min(SPhosts$cpu), min(SPhosts$mem), min(SPhosts$str))
colnames(minHosts) <- c("cpu", "mem", "str")
SPlinks <- decomposeSP(SP, "links")
minLinks <- data.frame(min(SPlinks$cap), min(SPlinks$del), min(SPlinks$jit))
colnames(minLinks) <- c("cap", "del", "jit")
SPnes <- decomposeSP(SP, "nes")
minNEs <- data.frame(min(SPnes$cap), min(SPnes$por), min(SPnes$que))
colnames(minNEs) <- c("cap", "por", "que")
# ending block of decomposing input demand #

# starting block of setting the price constraints per resources #
for ( m in 1:nrow(SPhosts) ) {
  priceHostPerDay <- priceHostPerDay + (24 * pricing(as.numeric(SPhosts[m,][1]), 
                                                     as.numeric(SPhosts[m,][2]), 
                                                     as.numeric(SPhosts[m,][3]), 
                                                     "hosts"))
}
priceHostPerDay <- priceHostPerDay * increasePriceFactor

for ( l in 1:nrow(SPlinks) ) {
  priceLinkPerDay <- priceLinkPerDay + (24 * pricing(as.numeric(SPlinks[l,][1]), 
                                                     as.numeric(SPlinks[l,][2]), 
                                                     as.numeric(SPlinks[l,][3]), 
                                                     "links"))
}
priceLinkPerDay <- priceLinkPerDay * increasePriceFactor

for ( n in 1:nrow(SPnes) ) {
  priceNEPerDay <- priceNEPerDay + (24 * pricing(as.numeric(SPnes[n,][1]), 
                                                 as.numeric(SPnes[n,][2]), 
                                                 as.numeric(SPnes[n,][3]), 
                                                 "nes"))
}
priceNEPerDay <- priceNEPerDay * increasePriceFactor

rm(l, m, n)
# ending block of setting the price constraints per resources #

costResults <- data.frame(matrix(ncol=5, nrow=0))


for (numberOfProviders in minNumberOfProviders:maxNumberOfProviders) {
  
  print(c(numberOfProviders, maxNumberOfProviders))
  turn <- 1
  
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
      Plinks <- decomposeProv(P, "links", minLinks)
      Pnes <- decomposeProv(P, "nes", minNEs)
      
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
          
          randomSample <- PhostsAux[randomRow, c("cpu", "mem", "str")]
          
          if ( all(SPhosts[demandID, c("cpu", "mem", "str")] <= randomSample) ) {
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
    
    # DUAS COISAS A SEREM IMPLEMENTADAS:
    # 01 - AS POLITICAS DE DISPONIBILIDADE E FIDELIDADE PODEM SER IMPLEMENTADAS 
    #      NO ARQUIVO .TXT ANTES DA OTIMIZACAO
    # 02 - UMA HEURISTICA ALEATORIA PODE SER IMPLEMENTADA A PARTIR DE TODAS OS 
    #      RECURSOS VALIDOS
    #      (SELECIONA ALEATORIAMENTE PARA CADA DEMANDA E REMOVE AS DEMAIS OFERTA
    #      PARA TAL DEMANA)
    #      (ALEM DE REMOVER AS DEMAIS ENTRADAS COM O MESMO CONJUNTO 
    #      <PROVEDOR,RECURSO>)
    
    for ( providerID in unique(Phosts$providerID) ) {
      
      finalStr <- "{"
      
      for (resourceID in Phosts[Phosts$providerID == providerID, ]$resourceID) {
        
        auxProvResource <- Phosts[Phosts$providerID == providerID & 
                                    Phosts$resourceID == resourceID, 
                                  c("cpu", "mem", "str", "price")]
        auxCond <- FALSE
        auxStr <- ""
        
        for ( demandID in 1:nrow(SPhosts) ) {
          
          if ( all( auxProvResource[c("cpu", "mem", "str")] >= 
                    SPhosts[demandID, c("cpu", "mem", "str")] ) ) {

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
      
      firstCost <- 0
      auxProvID <- 0; auxResID <- 0
      for ( i in 1:nrow(firstOfferHosts) ) {
        auxProvID <- firstOfferHosts[i,]$providerID
        auxResID <- firstOfferHosts[i,]$resourceID
        firstCost <- firstCost + (Phosts[Phosts$providerID == auxProvID & 
                                        Phosts$resourceID == auxResID,]$price) 
      }
      
      if ( optCost > firstCost ) { 
        print("ERROR: optimized cost greater than first choice!!!") 
      }
      
      costResults <- rbind(costResults, c(numberOfProviders, turn, 
                                          optCost, firstCost, randomCost))
      turn <- turn + 1
      rm(optCost, providerID, resourceID, responseOpt, firstCost, 
         auxResID, auxProvID, randomCost)
      
    }
    rm(solverCommand, logSolver)
    rm(numberOfTrials, P, Phosts, Plinks, Pnes, satisfied, foundFirstOfferHosts,
       firstOfferHosts, lastDemandHosts, lastResourceHosts, lastProviderHosts)
    
    #starting block for first and optimized strategies #
    
    # ending block for hosts #
    
  }
  
}

colnames(costResults) <- c("provs", "turn", "opt", "first", "random")
costSummary <- data.frame(matrix(ncol = 5, nrow = 0), stringsAsFactors = FALSE)
for ( i in unique(costResults$provs) ) { 

  # '1' means optimized cost
  costSummary <- rbind( costSummary, 
                  c(i, 1, Rmisc::CI(costResults[costResults$provs == i,]$opt)) )
  # '2' means first cost
  costSummary <- rbind( costSummary, 
                c(i, 2, Rmisc::CI(costResults[costResults$provs == i,]$first)) )
  # '3' means random cost
  costSummary <- rbind( costSummary, 
               c(i, 3, Rmisc::CI(costResults[costResults$provs == i,]$random)) )
  
}
colnames(costSummary) <- c("provs", "type", "upper", "mean", "lower")
rm(i)

# costPlot <- ggplot(costSummary, aes(x=provs, y=mean, ymin=lower, ymax=upper, 
#                                     group=type, color = factor(type))) +
#   theme_bw() +
#   geom_point() +
#   geom_smooth(stat="identity") +
#   theme(legend.position = "top") +
#   xlab("Number of Providers") + ylab("Average slice cost") +
#   scale_color_discrete("Legend:", breaks = unique(factor(costSummary$type)), 
#                        labels = c("opt", "first", "random"))
#ggsave(paste("h", "-", SPConfig[1], "-", SPConfig[2], "-", 
#SPConfig[3], ".pdf", sep = ""), plot = costPlot, device = "pdf")

rm(list=setdiff(ls(), ls(pattern = "cost")))
save.image()

print(paste("#time end", Sys.time(), sep = " "))