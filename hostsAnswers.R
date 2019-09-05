hostsAnswers <- function(hostsCombinations, pricePerDay) {

  auxDF <- data.frame()
  price <- vector()
  
  for (i in 1:nrow(hostsCombinations)) {
    
    leftSide <- as.numeric(hostsCombinations[i,1:(ncol(hostsCombinations)/2)])
    rightSide <- as.numeric(hostsCombinations[i,(ncol(hostsCombinations)/2+1):ncol(hostsCombinations)])
    
    if ( all(SPhosts[leftSide,] <= Phosts[rightSide, c("cpu", "mem", "str")], TRUE) ) {
      
      priceAux <- vector()
      
      for ( j in 1:length(rightSide) ) {
        
        arg1 <- Phosts[rightSide[j],"cpu"]
        arg2 <- Phosts[rightSide[j],"mem"]
        arg3 <- Phosts[rightSide[j],"str"]
        priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "hosts")*24 )
        
      }
      
      if ( all(priceAux <= pricePerDay, TRUE) ) {
        price <- c( price, sum(priceAux) )
        auxDF <- rbind(auxDF, hostsCombinations[i,])
      }

    }
    
  }
   
  auxDF <- cbind(auxDF, price)
  auxDF

}