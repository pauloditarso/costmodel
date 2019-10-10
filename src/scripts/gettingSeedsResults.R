for ( i in 1:20 ) {
  
  load(paste("./seed-", i, "/.RData", sep = ""))
  
  auxName <- paste("resultsSeed_", i, sep = "")
  assign(auxName, results)
  
  rm(list = ls(pattern = "results_"))
  rm(results)
  
}

rm(i, auxName)