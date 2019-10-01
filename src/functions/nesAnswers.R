nesAnswers <- function(nesCombinations, pricePerDay, pricingType) {
  
  auxDF <- data.frame()
  price <- vector()
  
  if (pricingType == "fixed") {
    
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
    
  }
  if (pricingType == "penalized") {
    
    for (i in 1:nrow(nesCombinations)) {
      
      leftSide <- as.numeric(nesCombinations[i,1:(ncol(nesCombinations)/2)])
      rightSide <- as.numeric(nesCombinations[i,(ncol(nesCombinations)/2+1):ncol(nesCombinations)])
      
      if ( all(SPnes[leftSide,] <= Pnes[rightSide, c("cap", "por", "que")], TRUE) ) {
        
        priceAux <- vector()
        lastProvider <- 0
        penalty <- 1.1
        
        for ( j in 1:length(rightSide) ) {
          
          actualProvider <- Pnes[rightSide[j], "providerID"]
          arg1 <- Pnes[rightSide[j],"cap"]
          arg2 <- Pnes[rightSide[j],"por"]
          arg3 <- Pnes[rightSide[j],"que"]
          
          if (lastProvider == actualProvider) {
            priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "nes")*24*penalty)
            penalty <- penalty + 0.1
          }
          else {
            priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "nes")*24 )
          }
          
          lastProvider <- actualProvider
          
        }
        
        if ( all(priceAux <= pricePerDay, TRUE) ) {
          price <- c( price, sum(priceAux) )
          auxDF <- rbind(auxDF, nesCombinations[i,])
        }
        
      }
      
    }
    
  }
  if (pricingType == "prized") {
    
    for (i in 1:nrow(nesCombinations)) {
      
      leftSide <- as.numeric(nesCombinations[i,1:(ncol(nesCombinations)/2)])
      rightSide <- as.numeric(nesCombinations[i,(ncol(nesCombinations)/2+1):ncol(nesCombinations)])
      
      if ( all(SPnes[leftSide,] <= Pnes[rightSide, c("cap", "por", "que")], TRUE) ) {
        
        priceAux <- vector()
        lastProvider <- 0
        reward <- 0.9
        
        for ( j in 1:length(rightSide) ) {
          
          actualProvider <- Pnes[rightSide[j], "providerID"]
          arg1 <- Pnes[rightSide[j],"cap"]
          arg2 <- Pnes[rightSide[j],"por"]
          arg3 <- Pnes[rightSide[j],"que"]
          
          if (lastProvider == actualProvider) {
            priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "nes")*24*reward)
            
            if (reward > 0.5) {
              reward <- reward - 0.1  
            }
            
          }
          else {
            priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "nes")*24 )
          }
          
          lastProvider <- actualProvider
          
        }
        
        if ( all(priceAux <= pricePerDay, TRUE) ) {
          price <- c( price, sum(priceAux) )
          auxDF <- rbind(auxDF, nesCombinations[i,])
        }
        
      }
      
    }
    
  }
  
  if ( pricingType != "fixed" & pricingType != "penalized" & pricingType != "prized" ) {
    print("ERROR: invalid type of pricing functions!!!")
  }
  
  auxDF <- cbind(auxDF, price)
  auxDF
  
}