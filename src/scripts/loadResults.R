rm(list = ls())
demands <- c("02", "04", "08", "16", "32", "64")

allCosts <- data.frame(matrix(ncol = 6, nrow = 0))
allCostsCI <- data.frame(matrix(ncol = 6, nrow = 0))
allDisc <- data.frame(matrix(ncol=6, nrow=0))
allReps <- data.frame(matrix(ncol=6, nrow=0))
allRepsCI <- data.frame(matrix(ncol=6, nrow=0))

for ( demand in demands ) {
  
  load(paste("../experiments/", demand, "/.RData", sep = ""))
  allCosts <- rbind( allCosts, finalCosts)
  allCostsCI <- rbind( allCostsCI, finalCostsCI )
  allDisc <- rbind( allDisc, finalDisc )
  allReps <- rbind( allReps, finalReps )
  allRepsCI <- rbind( allRepsCI, finalRepsCI )
  rm( list = setdiff(ls(), ls(pattern = "all")) )
  
}

colnames(allCosts) <- c("demand", "provs", "turn", "opt", "first", "random")
colnames(allCostsCI) <- c("demand", "provs", "type", "upper", "mean", "lower")
colnames(allDisc) <- c("demand", "provs", "turn", "type", "target", "opt")
colnames(allReps) <- c("demand", "hosts", "provs", "turn", "used", "type")
colnames(allRepsCI) <- c("demand", "provs", "type", "upper", "mean", "lower")


allDiscCI <- data.frame(matrix(nrow = 0, ncol = 6))
discTypes <- c(4, 5, 6)
providers <- 5:20
demands <- unique(factor(allDisc$demand))
for ( demand in demands ) {

  for ( provider in providers ) {
    
    for ( discType in discTypes ) {
      
      targetAux <- as.numeric(allDisc[allDisc$demand == demand & allDisc$provs == provider & allDisc$type == discType,]$target)
      optAux <- as.numeric(allDisc[allDisc$demand == demand & allDisc$provs == provider & allDisc$type == discType,]$opt)
      allDiscCI <- rbind( allDiscCI, c(demand, provider, discType, Rmisc::CI(targetAux/optAux)) )
      
    }
    
  }

}
rm(demand, demands, provider, providers, discType, discTypes, optAux, targetAux)
colnames(allDiscCI) <- c("demand", "provs", "type", "upper", "mean", "lower")