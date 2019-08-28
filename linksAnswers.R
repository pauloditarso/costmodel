linksAnswers <- function(linksCombinations, pricePerDay) {
  
  auxDF <- data.frame()
  price <- vector()
  
  if ( !exists("providersFactor", envir = .GlobalEnv) ) providersFactor <- 0.0005
  
  for (i in 1:nrow(linksCombinations)) {
    
    leftSide <- as.numeric(linksCombinations[i,1:(ncol(linksCombinations)/2)])
    rightSide <- as.numeric(linksCombinations[i,(ncol(linksCombinations)/2+1):ncol(linksCombinations)])
    
    if ( all(SPlinks[leftSide,] <= Plinks[rightSide, c("cap", "del", "jit")], TRUE) ) {
      
      priceAux <- 0
      priceAux <- pricing(Plinks[rightSide,"cap"][[1]],Plinks[rightSide,"del"][[1]],Plinks[rightSide,"jit"][[1]],providersFactor)*24
      
      if ( priceAux <= pricePerDay ) {
        price <- c(price, priceAux)
        auxDF <- rbind(auxDF, linksCombinations[i,])
      }
      
    }
    
  }
  
  auxDF <- cbind(auxDF, price)
  auxDF
  
}