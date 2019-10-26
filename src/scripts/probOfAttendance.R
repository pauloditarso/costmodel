probOfAttendance <- function(numberOfTrials, numberOfProvs) {
  
  auxProb <- 0
  for (i in seeds) {
    
    auxName <- paste("resultsSeed_", i, sep = "")
    auxProb <- auxProb + (length(which(get(auxName)[,numberOfProvs] <= numberOfTrials))/1000)
    # print(paste("seed ", i, " provider ", numberOfProvs, " prob ", auxProb, sep = ""))

  }
  
  print(auxProb/length(seeds))
}