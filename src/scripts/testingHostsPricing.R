source('~/Workspace/R-workspace/marketplace/costmodel/src/functions/pricing.R')

hostsPricing <- vector()
referenceHost <- c(1, 4, 128, 0.1)
referenceLink <- c(1, 1, 1, 2)
referenceNE <- c(1, 6, 1, 2)

for ( cpu in c(1, 2, 4, 8, 16, 32) ) {
  for ( mem in c(4, 8, 16, 32, 64, 128) ) {
    for ( str in c(128, 256, 512, 1024, 2048, 4096) ) {
      hostsPricing <- c(hostsPricing, pricing(cpu, mem, str, "hosts"))
    }
  }
}

rm( list = setdiff(ls(), ls(pattern = "Pricing")) )