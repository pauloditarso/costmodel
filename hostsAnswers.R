hostsAnswers <- function(hostsCombinations, pricePerDay) {

  auxDF <- data.frame()
  if ( !exists("providersFactor", envir = .GlobalEnv) ) providersFactor <- 0.0005
  
  for (i in 1:nrow(hostsCombinations)) {
    
    leftSide <- as.numeric(hostsCombinations[i,1:(ncol(hostsCombinations)/2)])
    rightSide <- as.numeric(hostsCombinations[i,(ncol(hostsCombinations)/2+1):ncol(hostsCombinations)])
    
    if ( all(SPhosts[leftSide,] <= Phosts[rightSide, c("cpu", "mem", "str")], TRUE) ) {
      
      if ( pricing(Phosts[rightSide,"cpu"][[1]],Phosts[rightSide,"mem"][[1]],Phosts[rightSide,"str"][[1]],providersFactor)*24 <= pricePerDay ) {
        auxDF <- rbind(auxDF, hostsCombinations[i,])
      }

    }
    
  }
   
  auxDF

}