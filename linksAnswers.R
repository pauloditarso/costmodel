linksAnswers <- function(linksCombinations) {
  
  auxDF <- data.frame()
  
  for (i in 1:nrow(linksCombinations)) {
    
    leftSide <- as.numeric(linksCombinations[i,1:(ncol(linksCombinations)/2)])
    rightSide <- as.numeric(linksCombinations[i,(ncol(linksCombinations)/2+1):ncol(linksCombinations)])
    
    if ( all(SPlinks[leftSide,] <= Plinks[rightSide, c("cap", "del", "jit")], TRUE) ) {
      auxDF <- rbind(auxDF, linksCombinations[i,])
    }
    
  }
  
  auxDF
  
}