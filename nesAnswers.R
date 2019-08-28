nesAnswers <- function(nesCombinations, pricePerDay) {
  
  auxDF <- data.frame()
  price <- vector()
  
  if ( !exists("providersFactor", envir = .GlobalEnv) ) providersFactor <- 0.0005
  
  for (i in 1:nrow(nesCombinations)) {
    
    leftSide <- as.numeric(nesCombinations[i,1:(ncol(nesCombinations)/2)])
    rightSide <- as.numeric(nesCombinations[i,(ncol(nesCombinations)/2+1):ncol(nesCombinations)])
    
    if ( all(SPnes[leftSide,] <= Pnes[rightSide, c("cap", "por", "que")], TRUE) ) {
      
      priceAux <- 0
      priceAux <- pricing(Pnes[rightSide,"cap"][[1]],Pnes[rightSide,"por"][[1]],Pnes[rightSide,"que"][[1]],providersFactor)*24
      
      if ( priceAux <= pricePerDay ) {
        price <- c(price, priceAux)
        auxDF <- rbind(auxDF, nesCombinations[i,])
      }
      
    }
    
  }
  
  auxDF <- cbind(auxDF, price)
  auxDF
  
}