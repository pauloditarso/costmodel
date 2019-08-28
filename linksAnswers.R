linksAnswers <- function(linksCombinations, pricePerDay) {
  
  auxDF <- data.frame()
  if ( !exists("providersFactor", envir = .GlobalEnv) ) providersFactor <- 0.0005
  
  for (i in 1:nrow(linksCombinations)) {
    
    leftSide <- as.numeric(linksCombinations[i,1:(ncol(linksCombinations)/2)])
    rightSide <- as.numeric(linksCombinations[i,(ncol(linksCombinations)/2+1):ncol(linksCombinations)])
    
    if ( all(SPlinks[leftSide,] <= Plinks[rightSide, c("cap", "del", "jit")], TRUE) ) {
      
      if ( pricing(Plinks[rightSide,"cap"][[1]],Plinks[rightSide,"del"][[1]],Plinks[rightSide,"jit"][[1]],providersFactor)*24 <= pricePerDay ) {
        auxDF <- rbind(auxDF, linksCombinations[i,])
      }
      
    }
    
  }
  
  auxDF
  
}