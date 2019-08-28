source('./sourceAll.R')

if ( exists("SPConfig") ) {
  SP <- createOneSP(SPConfig[1], SPConfig[2], SPConfig[3])
} else {
  SP <- createOneSP()
}

SPhosts <- decomposeSP(SP, "hosts")
SPlinks <- decomposeSP(SP, "links")
SPnes <- decomposeSP(SP, "nes")

if ( exists("numberOfProviders") ) {
  P <- createProviders(numberOfProviders)
} else {
  P <- createProviders(2)  
}

Phosts <- decomposeProv(P, "hosts")
Plinks <- decomposeProv(P, "links")
Pnes <- decomposeProv(P, "nes")

hostsComb <- indexes(nrow(SPhosts), nrow(Phosts))
linksComb <- indexes(nrow(SPlinks), nrow(Plinks))
nesComb <- indexes(nrow(SPnes), nrow(Pnes))

hostsDF <- hostsAnswers(hostsComb, 25)
linksDF <- linksAnswers(linksComb, 25)
nesDF <- nesAnswers(nesComb, 25)

if ( nrow(hostsDF) == 0 | nrow(linksDF) == 0 | nrow(nesDF) == 0 ) { print("There is no valid answer!!") }