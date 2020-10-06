rm(list = ls())
library(ggplot2)
print(paste("#time start", Sys.time(), sep = " "))
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
  
  if( length(SP$hosts) == SPConfig[1] & length(SP$links) == SPConfig[2] & length(SP$nes) == SPConfig[3] ) {
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
  priceHostPerDay <- priceHostPerDay + (24 * pricing(as.numeric(SPhosts[m,][1]), as.numeric(SPhosts[m,][2]), as.numeric(SPhosts[m,][3]), "hosts"))
}
priceHostPerDay <- priceHostPerDay * increasePriceFactor

for ( l in 1:nrow(SPlinks) ) {
  priceLinkPerDay <- priceLinkPerDay + (24 * pricing(as.numeric(SPlinks[l,][1]), as.numeric(SPlinks[l,][2]), as.numeric(SPlinks[m,][3]), "links"))
}
priceLinkPerDay <- priceLinkPerDay * increasePriceFactor

for ( n in 1:nrow(SPnes) ) {
  priceNEPerDay <- priceNEPerDay + (24 * pricing(as.numeric(SPnes[n,][1]), as.numeric(SPnes[n,][2]), as.numeric(SPnes[n,][3]), "nes"))
}
priceNEPerDay <- priceNEPerDay * increasePriceFactor

rm(l, m, n)
# ending block of setting the price constraints per resources #

costResults <- data.frame(matrix(ncol=4, nrow=0))


for (numberOfProviders in minNumberOfProviders:maxNumberOfProviders) {
  
  print(c(numberOfProviders, maxNumberOfProviders))
  turn <- 1
  
  while (turn <= numberOfTurns) {
    
    satisfied <- FALSE
    numberOfTrials <- 0
    while (!satisfied) {
      
      numberOfTrials <- numberOfTrials + 1
      if (numberOfTrials == 1000) { stop("ERROR: infinite loop!!!") }
      
      P <- createProviders(numberOfProviders, SPConfig[1], SPConfig[2], SPConfig[3], minHosts, minLinks, minNEs)
      
      Phosts <- decomposeProv(P, "hosts", minHosts)
      Plinks <- decomposeProv(P, "links", minLinks)
      Pnes <- decomposeProv(P, "nes", minNEs)
      
      if ( nrow(Phosts) >= nrow(SPhosts) & nrow(Plinks) >= nrow(SPlinks) & nrow(Pnes) >= nrow(SPnes) ) {
        satisfied <- TRUE
      }
      
    }
    
    scenarioName <- paste("h", "-", SPConfig[1], "-", SPConfig[2], "-", SPConfig[3], "-", numberOfProviders, sep = "")
    
    ####### confs for hosts #######
    if ( length(unique(Phosts$providerID)) < numberOfProviders ) {
      rm(list = ls())
      stop("ERROR: less providers than original amount!!!")
    }
    
    fd <- file(paste("./files/", scenarioName, ".txt", sep = "" ), "w")
    writeLines( noquote(paste(length(unique(Phosts$providerID)), nrow(SPhosts), sep = " ")), con = fd, sep = "\n" )
  
    firstOfferHosts <- data.frame(matrix(nrow = 0, ncol = 3))
    foundFirstOfferHosts <- FALSE
    lastDemand <- 0
    lastResource <- 0
    lastProvider <- 0
    
    for ( providerID in unique(Phosts$providerID) ) {
      
      finalStr <- "{"
      
      for ( resourceID in Phosts[Phosts$providerID == providerID, ]$resourceID ) {
        
        auxProvResource <- Phosts[Phosts$providerID == providerID & Phosts$resourceID == resourceID, c("cpu", "mem", "str", "price")]
        auxCond <- FALSE
        auxStr <- ""
        
        for ( demandID in 1:nrow(SPhosts) ) {
          
          if ( all( auxProvResource[c("cpu", "mem", "str")] >= SPhosts[demandID, c("cpu", "mem", "str")] ) ) {

            if ( nrow(firstOfferHosts) == nrow(SPhosts) ) {
              foundFirstOfferHosts <- TRUE
            }            
            if ( (!foundFirstOfferHosts) & (lastDemand < demandID) ) {
              
              if (lastProvider != providerID | lastResource != resourceID) {
                firstOfferHosts <- rbind( firstOfferHosts, c(providerID, resourceID, demandID) )
                lastDemand <- demandID
                lastResource <- resourceID
                lastProvider <- providerID
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
        
        if ( auxCond == TRUE ) { finalStr <- paste(finalStr, auxStr, ":", auxProvResource["price"], sep = " ") }
        
        if ( resourceID != tail(Phosts[Phosts$providerID == providerID, ]$resourceID, n=1)) {
          finalStr <- paste(finalStr, ";", sep = " ")
        }
        else {
          finalStr <- paste(finalStr, "}", sep = " ") 
        }
        
      }
      
      writeLines( noquote(finalStr), con = fd, sep = "\n" )
    }
    close(fd)
    rm(fd, providerID, resourceID, demandID, finalStr, auxCond, auxStr, auxProvResource)
    
    bashCommand <- paste("bash files/count.sh files/", scenarioName, ".txt", sep = "")
    testBash <- as.numeric(system(bashCommand, intern = TRUE))
    testProv <- tabulate(Phosts$providerID)
    if (any(testBash != testProv)) { print("ERROR: tests are different!!!") }
    rm(bashCommand, testBash, testProv)
    
    solverCommand <- paste("bash solver/gera.sh files/", scenarioName, ".txt", sep = "" )
    logSolver <- system(solverCommand, intern = TRUE)
    if ( file.info(paste("files/", scenarioName, ".opt", sep = ""))$size != 0 & nrow(firstOfferHosts) == nrow(SPhosts) ) {
      
      responseOpt <- read.csv(paste("files/", scenarioName, ".opt", sep = ""), header = F)
      
      costScenario <- 0
      providerID <- 0
      resourceID <- 0
      for (i in 1:nrow(responseOpt)) { 
        providerID <- responseOpt[i,]$V1
        resourceID <- responseOpt[i,]$V2
        costScenario <- costScenario + 
          as.numeric(Phosts[Phosts$providerID == providerID & 
                              Phosts$resourceID == resourceID,]$price)
      }
      
      firstCost <- 0
      auxProvID <- 0; auxResID <- 0
      for ( i in 1:nrow(firstOfferHosts) ) {
        auxProvID <- firstOfferHosts[i,]$providerID
        auxResID <- firstOfferHosts[i,]$resourceID
        firstCost <- firstCost + (Phosts[Phosts$providerID == auxProvID & Phosts$resourceID == auxResID,]$price) 
      }
      
      if ( costScenario > firstCost ) { print("ERROR: minimal cost greater than first!!!") }
      
      costResults <- rbind(costResults, c(numberOfProviders, turn, costScenario, firstCost))
      turn <- turn + 1
      rm(costScenario, providerID, resourceID, responseOpt, firstCost, auxResID, auxProvID)
      
    }
    rm(solverCommand, logSolver)
    rm(numberOfTrials, P, Phosts, Plinks, Pnes, satisfied, foundFirstOfferHosts, 
       firstOfferHosts, lastDemand, lastResource, lastProvider)

  }
  
}

colnames(costResults) <- c("provs", "turn", "min", "first")
costSummary <- data.frame(matrix(ncol = 5, nrow = 0), stringsAsFactors = FALSE)
for ( i in unique(costResults$provs) ) { 

  costSummary <- rbind( costSummary, c(i, 1, Rmisc::CI(costResults[costResults$provs == i,]$min)) )
  costSummary <- rbind( costSummary, c(i, 2, Rmisc::CI(costResults[costResults$provs == i,]$first)) )
  
}
colnames(costSummary) <- c("provs", "cost", "upper", "mean", "lower")
rm(i)

# p <- ggplot2::ggplot(costSummary, aes(x=provs, y=mean)) +
#    geom_point() +
#    geom_errorbar(aes(ymax = upper, ymin = lower), width=.2)
# 
# ggsave("out.pdf", plot = p, device = "pdf")

costPlot <- ggplot2::ggplot(costSummary, aes(x=provs, y=mean, ymin=lower, ymax=upper, color = factor(cost))) +
  geom_point() +
#  geom_smooth(method = "glm", se = TRUE)
#geom_smooth(method = glm, aes(ymin=lower, ymax=upper, fill=cost), se = TRUE)
geom_errorbar(width=.2)
ggsave(paste("h", "-", SPConfig[1], "-", SPConfig[2], "-", SPConfig[3], ".pdf", sep = ""), plot = costPlot, device = "pdf")

rm(list=setdiff(ls(), ls(pattern = "cost")))
save.image()

print(paste("#time end", Sys.time(), sep = " "))