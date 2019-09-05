nesAnswers <- function(nesCombinations, pricePerDay) {
  
  auxDF <- data.frame()
  price <- vector()
  
  for (i in 1:nrow(nesCombinations)) {
    
    leftSide <- as.numeric(nesCombinations[i,1:(ncol(nesCombinations)/2)])
    rightSide <- as.numeric(nesCombinations[i,(ncol(nesCombinations)/2+1):ncol(nesCombinations)])
    
    if ( all(SPnes[leftSide,] <= Pnes[rightSide, c("cap", "por", "que")], TRUE) ) {
      
      priceAux <- vector()
      
      for ( j in 1:length(rightSide) ) {
        
        arg1 <- Pnes[rightSide[j],"cap"]
        arg2 <- Pnes[rightSide[j],"por"]
        arg3 <- Pnes[rightSide[j],"que"]
        priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "nes")*24 )
        
      }
      
      if ( all(priceAux <= pricePerDay, TRUE) ) {
        price <- c( price, sum(priceAux) )
        auxDF <- rbind(auxDF, nesCombinations[i,])
      }
      
    }
    
  }
  
  auxDF <- cbind(auxDF, price)
  auxDF
  
}