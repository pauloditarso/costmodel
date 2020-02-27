rm(list = ls())
source('./src/scripts/sourceAll.R')
SPConfig<-c(2,2,1)
numberOfProviders <- 10
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

P <- createProviders(numberOfProviders, SPConfig[1], SPConfig[2], SPConfig[3])

Phosts <- decomposeProv(P, "hosts", minHosts)
Plinks <- decomposeProv(P, "links", minLinks)
Pnes <- decomposeProv(P, "nes", minNEs)

hostsComb <- indexes(nrow(SPhosts), nrow(Phosts))
linksComb <- indexes(nrow(SPlinks), nrow(Plinks))
nesComb <- indexes(nrow(SPnes), nrow(Pnes))

hostsDF <- hostsAnswers(hostsComb, priceHostPerDay, pricingType)
linksDF <- linksAnswers(linksComb, priceLinkPerDay, pricingType)
nesDF <- nesAnswers(nesComb, priceNEPerDay, pricingType)