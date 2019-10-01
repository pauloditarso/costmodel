#rm(list = ls())
source('./src/scripts/sourceAll.R')
SPConfig<-c(2,2,1)
#numberOfProviders<-2
priceHostPerDay<-100
priceLinkPerDay<-100
priceNEPerDay<-100
pricingType <- "fixed"

#print("defining reference resources!!")
referenceHost <- c(1, 4, 128, 0.1)
referenceLink <- c(1, 1, 4, 0.2)
referenceNE <- c(1, 6, 1, 0.2)

if ( !exists("priceHostPerDay") ) {
  priceHostPerDay <- 25
}
if ( !exists("priceLinkPerDay") ) {
  priceLinkPerDay <- 25
}
if ( !exists("priceNEPerDay") ) {
  priceNEPerDay <- 25
}

#print("defining SP resources!!")
if ( exists("SPConfig") ) {
  SP <- createOneSP(SPConfig[1], SPConfig[2], SPConfig[3])
} else {
  SP <- createOneSP()
}

SPhosts <- decomposeSP(SP, "hosts")
SPlinks <- decomposeSP(SP, "links")
SPnes <- decomposeSP(SP, "nes")

#print("defining providers resources!!")
if ( exists("numberOfProviders") ) {
  P <- createProviders(numberOfProviders, 4, 4, 2)
} else {
  P <- createProviders(2)  
}

Phosts <- decomposeProv(P, "hosts")
#print(paste(nrow(Phosts), "hosts"))
Plinks <- decomposeProv(P, "links")
#print(paste(nrow(Plinks), "links"))
Pnes <- decomposeProv(P, "nes")
#print(paste(nrow(Pnes), "nes"))

#print("defining hosts combinations!!")
hostsComb <- indexes(nrow(SPhosts), nrow(Phosts))
#print("defining links combinations!!")
linksComb <- indexes(nrow(SPlinks), nrow(Plinks))
#print("defining nes combinations!!")
nesComb <- indexes(nrow(SPnes), nrow(Pnes))

#print("defining hosts answers!!")
hostsDF <- hostsAnswers(hostsComb, priceHostPerDay, pricingType)
#print("defining links answers!!")
linksDF <- linksAnswers(linksComb, priceLinkPerDay, pricingType)
#print("defining nes answers!!")
nesDF <- nesAnswers(nesComb, priceNEPerDay, pricingType)

if ( nrow(hostsDF) == 0 | nrow(linksDF) == 0 | nrow(nesDF) == 0 ) { 
  failed <- TRUE
} else {
  failed <- FALSE
}