rm(list = ls())
source('./src/scripts/sourceAll.R')
SPConfig<-c(2,2,1)
numberOfProviders <- 40
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
SPlinks <- decomposeSP(SP, "links")
SPnes <- decomposeSP(SP, "nes")

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