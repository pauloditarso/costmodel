hostsAnswers <- function(hostsCombinations) {

  auxDF <- data.frame()
  
  for (i in 1:nrow(hostsCombinations)) {
    
    leftSide <- as.numeric(hostsCombinations[i,1:(ncol(hostsCombinations)/2)])
    rightSide <- as.numeric(hostsCombinations[i,(ncol(hostsCombinations)/2+1):ncol(hostsCombinations)])
    
    if ( all(SPhosts[leftSide,] <= Phosts[rightSide, c("cpu", "mem", "str")], TRUE) ) {
      auxDF <- rbind(auxDF, hostsCombinations[i,])
    }
    
  }
   
  auxDF

}