probOfSuccessDists <- function() {
  
  auxProb <- data.frame(matrix(0, nrow = 47, ncol = 20))
  
  for (i in 1:20) {
    
    for (j in seeds) {
      
      auxName <- paste("resultsSeed_", j, sep = "")    
      auxProb[j,i] <- as.numeric(fitdistr((get(auxName)[,i]-1), densfun = "geometric")$estimate)
      
    }
    
  }

  colnames(auxProb) <- c(1:20)  
  print(auxProb)
  
}