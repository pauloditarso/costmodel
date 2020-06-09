rm(list = ls())
print(paste("#time start", Sys.time(), sep = " "))
source('./src/scripts/sourceAll.R')
SPConfig<-c(2,2,1)
minNumberOfProviders <- 5
maxNumberOfProviders <- 10
numberOfTurns <- 30
priceHostPerDay <- 0
priceLinkPerDay <- 0
priceNEPerDay <- 0
pricingType <- "fixed"
quota <- -1

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

rm(l, m, n)

numberOfProviders <- 3

P <- createProviders(numberOfProviders, SPConfig[1], SPConfig[2], SPConfig[3])

Phosts <- decomposeProv(P, "hosts", minHosts)
Plinks <- decomposeProv(P, "links", minLinks)
Pnes <- decomposeProv(P, "nes", minNEs)

for ( providerID in unique(Phosts$providerID) ) {

  for ( resourceID in Phosts[Phosts$providerID == providerID, ]$resourceID ) {
    
    aux <- Phosts[Phosts$providerID == providerID & Phosts$resourceID == resourceID, c("cpu", "mem", "str")]
    
    for ( demandID in 1:nrow(SPhosts) ) {
      
      if ( all( aux >= SPhosts[demandID, c("cpu", "mem", "str")] ) ) {
        print(aux[demandID,])
      }
      
    }
    
  }

}
rm(aux)

print(paste("#time end", Sys.time(), sep = " "))

