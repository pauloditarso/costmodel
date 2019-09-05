linksAnswers <- function(linksCombinations, pricePerDay) {
  
  auxDF <- data.frame()
  price <- vector()
  
  for (i in 1:nrow(linksCombinations)) {
    
    leftSide <- as.numeric(linksCombinations[i,1:(ncol(linksCombinations)/2)])
    rightSide <- as.numeric(linksCombinations[i,(ncol(linksCombinations)/2+1):ncol(linksCombinations)])
    
    if ( all(SPlinks[leftSide,] <= Plinks[rightSide, c("cap", "del", "jit")], TRUE) ) {
      
      priceAux <- vector()
      
      for ( j in 1:length(rightSide) ) {
        
        arg1 <- Plinks[rightSide[j],"cap"]
        arg2 <- Plinks[rightSide[j],"del"]
        arg3 <- Plinks[rightSide[j],"jit"]
        priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "links")*24 )
        
      }
      
      if ( all(priceAux <= pricePerDay, TRUE) ) {
        price <- c( price, sum(priceAux) )
        auxDF <- rbind(auxDF, linksCombinations[i,])
      }
      
    }
    
  }
  
  auxDF <- cbind(auxDF, price)
  auxDF
  
}