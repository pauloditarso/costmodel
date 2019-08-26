nesAnswers <- function(nesCombinations) {
  
  auxDF <- data.frame()
  
  for (i in 1:nrow(nesCombinations)) {
    
    leftSide <- as.numeric(nesCombinations[i,1:(ncol(nesCombinations)/2)])
    rightSide <- as.numeric(nesCombinations[i,(ncol(nesCombinations)/2+1):ncol(nesCombinations)])
    
    if ( all(SPnes[leftSide,] <= Pnes[rightSide, c("cap", "por", "que")], TRUE) ) {
      auxDF <- rbind(auxDF, nesCombinations[i,])
    }
    
  }
  
  auxDF
  
}