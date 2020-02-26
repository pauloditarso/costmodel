source('~/Workspace/R-workspace/marketplace/costmodel/src/functions/pricing.R')

linksPricing <- vector()
referenceHost <- c(1, 4, 128, 0.1)
referenceLink <- c(1, 1, 1, 2)
referenceNE <- c(1, 6, 1, 2)

for ( i in c(1, 2, 3, 4) ) {
  for ( j in c(1, 2, 3, 4) ) {
    for ( k in c(1, 2, 3, 4) ) {
      linksPricing <- c(linksPricing, pricing(i, j, k, "links"))
    }
  }
}

rm( list = setdiff(ls(), ls(pattern = "Pricing")) )