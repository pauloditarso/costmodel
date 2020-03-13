rm(list = ls())
print(paste("#time start", Sys.time(), sep = " "))
start_time <- Sys.time()
source('./src/scripts/sourceAll.R')
SPConfig<-c(4,4,2)
numberOfProviders <- 3
numberOfTurns <- 30
#seed <- 1
priceHostPerDay <- 0
priceLinkPerDay <- 0
priceNEPerDay <- 0
pricingType <- "fixed"

referenceHost <- c(1, 4, 128, 0.1)
referenceLink <- c(1, 1, 1, 2)
referenceNE <- c(1, 6, 1, 2)

#set.seed(seed)
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

# setting the price constraints per resources
for ( m in 1:nrow(SPhosts) ) {
  priceHostPerDay <- priceHostPerDay + (24 * pricing(as.numeric(SPhosts[m,][1]), as.numeric(SPhosts[m,][2]), as.numeric(SPhosts[m,][3]), "hosts"))
}
priceHostPerDay <- priceHostPerDay*1.5
for ( l in 1:nrow(SPlinks) ) {
  priceLinkPerDay <- priceLinkPerDay + (24 * pricing(as.numeric(SPlinks[l,][1]), as.numeric(SPlinks[l,][2]), as.numeric(SPlinks[m,][3]), "links"))
}
priceLinkPerDay <- priceLinkPerDay*1.5
for ( n in 1:nrow(SPnes) ) {
  priceNEPerDay <- priceNEPerDay + (24 * pricing(as.numeric(SPnes[n,][1]), as.numeric(SPnes[n,][2]), as.numeric(SPnes[n,][3]), "nes"))
}
priceNEPerDay <- priceNEPerDay*1.5


for ( i in 1:numberOfTurns ) {
  
  valid <- FALSE
  seed <- as.numeric(Sys.time())
  
  while(!valid)  {

    set.seed(seed)

    P <- createProviders(numberOfProviders, SPConfig[1], SPConfig[2], SPConfig[3])
    
    Phosts <- decomposeProv(P, "hosts", minHosts)
    Plinks <- decomposeProv(P, "links", minLinks)
    Pnes <- decomposeProv(P, "nes", minNEs)

    hostsComb <- indexes(nrow(SPhosts), nrow(Phosts))
    linksComb <- indexes(nrow(SPlinks), nrow(Plinks))
    nesComb <- indexes(nrow(SPnes), nrow(Pnes))

    if ( nrow(hostsComb) != 0 & nrow(linksComb) != 0 & nrow(nesComb) != 0 ) {
      hostsDF <- hostsAnswers(hostsComb, priceHostPerDay, pricingType)
      linksDF <- linksAnswers(linksComb, priceLinkPerDay, pricingType)
      nesDF <- nesAnswers(nesComb, priceNEPerDay, pricingType)
    } else {
      hostsDF <- data.frame()
      linksDF <- data.frame()
      nesDF <- data.frame()
    }
    
    if ( nrow(hostsDF) == 0 | nrow(linksDF) == 0 | nrow(nesDF) == 0 ) {
      #seed <- (seed + sample(1:1000,1))
      seed <- as.numeric(Sys.time())
      source('./src/scripts/desourceAll.R')
    } else {
      valid <- TRUE
    }
    
  }
  
  hostsResults <- resultsComputation("hosts")
  linksResults <- resultsComputation("links")
  nesResults <- resultsComputation("nes")
  
  auxHosts <- paste("hostsResults_", i, sep = "")
  assign(auxHosts, hostsResults, envir = .GlobalEnv)
  rm(auxHosts, hostsResults)
  
  auxLinks <- paste("linksResults_", i, sep = "")
  assign(auxLinks, linksResults, envir = .GlobalEnv)
  rm(auxLinks, linksResults)
  
  auxNEs <- paste("nesResults_", i, sep = "")
  assign(auxNEs, nesResults, envir = .GlobalEnv)
  rm(auxNEs, nesResults)
  
  print(paste("#turn", i, "time", Sys.time(), sep = " "))
}

# resultsList <- ls(pattern = "_")
# rm(list = setdiff(ls(), resultsList))

hostsPrices <- vector()
linksPrices <- vector()
nesPrices <- vector()

for (i in 1:numberOfTurns) {
  
  hostsPrices[i] <- min(get(paste("hostsResults_", i, sep = ""))$price)
  linksPrices[i] <- min(get(paste("linksResults_", i, sep = ""))$price)
  nesPrices[i] <- min(get(paste("nesResults_", i, sep = ""))$price)
}

end_time <- Sys.time()
total_time <- (end_time - start_time)
print(total_time)
print(paste("#time end", Sys.time(), sep = " "))
save.image()