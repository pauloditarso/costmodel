linksAnswers <- function(linksCombinations, pricePerDay, pricingType) {
  
  auxDF <- data.frame()
  price <- vector()
  
  if (pricingType == "fixed") {
    
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
    
  }
  if (pricingType == "penalized") {
    
    for (i in 1:nrow(linksCombinations)) {
      
      leftSide <- as.numeric(linksCombinations[i,1:(ncol(linksCombinations)/2)])
      rightSide <- as.numeric(linksCombinations[i,(ncol(linksCombinations)/2+1):ncol(linksCombinations)])
      
      if ( all(SPlinks[leftSide,] <= Plinks[rightSide, c("cap", "del", "jit")], TRUE) ) {
        
        priceAux <- vector()
        lastProvider <- 0
        penalty <- 1.1
        
        for ( j in 1:length(rightSide) ) {
          
          actualProvider <- Plinks[rightSide[j], "providerID"]
          arg1 <- Plinks[rightSide[j],"cap"]
          arg2 <- Plinks[rightSide[j],"del"]
          arg3 <- Plinks[rightSide[j],"jit"]
          
          if (lastProvider == actualProvider) {
            priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "links")*24*penalty)
            penalty <- penalty + 0.1
          }
          else {
            priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "links")*24 )
          }
          
          lastProvider <- actualProvider
          
        }
        
        if ( all(priceAux <= pricePerDay, TRUE) ) {
          price <- c( price, sum(priceAux) )
          auxDF <- rbind(auxDF, linksCombinations[i,])
        }
        
      }
      
    }
    
  }
  if (pricingType == "prized") {
    
    for (i in 1:nrow(linksCombinations)) {
      
      leftSide <- as.numeric(linksCombinations[i,1:(ncol(linksCombinations)/2)])
      rightSide <- as.numeric(linksCombinations[i,(ncol(linksCombinations)/2+1):ncol(linksCombinations)])
      
      if ( all(SPlinks[leftSide,] <= Plinks[rightSide, c("cap", "del", "jit")], TRUE) ) {
        
        priceAux <- vector()
        lastProvider <- 0
        reward <- 0.9
        
        for ( j in 1:length(rightSide) ) {
          
          actualProvider <- Plinks[rightSide[j], "providerID"]
          arg1 <- Plinks[rightSide[j],"cap"]
          arg2 <- Plinks[rightSide[j],"del"]
          arg3 <- Plinks[rightSide[j],"jit"]
          
          if (lastProvider == actualProvider) {
            priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "links")*24*reward)
            
            if (reward > 0.5) {
              reward <- reward - 0.1  
            }
            
          }
          else {
            priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "links")*24 )
          }
          
          lastProvider <- actualProvider
          
        }
        
        if ( all(priceAux <= pricePerDay, TRUE) ) {
          price <- c( price, sum(priceAux) )
          auxDF <- rbind(auxDF, linksCombinations[i,])
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