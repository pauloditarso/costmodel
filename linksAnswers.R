linksAnswers <- function(linksCombinations, pricePerDay) {
  
  auxDF <- data.frame()
  price <- vector()
  
  #  if ( !exists("providersFactor", envir = .GlobalEnv) ) providersFactor <- 0.0005
  
  for (i in 1:nrow(linksCombinations)) {
    
    leftSide <- as.numeric(linksCombinations[i,1:(ncol(linksCombinations)/2)])
    rightSide <- as.numeric(linksCombinations[i,(ncol(linksCombinations)/2+1):ncol(linksCombinations)])
    
    if ( all(SPlinks[leftSide,] <= Plinks[rightSide, c("cap", "del", "jit")], TRUE) ) {
      
      priceAux <- 0
      arg1 <- Plinks[rightSide,"cap"][[1]]
      arg2 <- Plinks[rightSide,"del"][[1]]
      arg3 <- Plinks[rightSide,"jit"][[1]]
      priceAux <- pricing(arg1, arg2, arg3, "links")*24 
      
      if ( priceAux <= pricePerDay ) {
        price <- c(price, priceAux)
        auxDF <- rbind(auxDF, linksCombinations[i,])
      }
      
    }
    
  }
  
  auxDF <- cbind(auxDF, price)
  auxDF
  
}