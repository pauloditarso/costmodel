hostsAnswers <- function(hostsCombinations, pricePerDay, pricingType) {

  auxDF <- data.frame()
  price <- vector()
  
  if (pricingType == "fixed") {
    
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
    
  }
  if (pricingType == "penalized") {
    
    for (i in 1:nrow(hostsCombinations)) {
      
      leftSide <- as.numeric(hostsCombinations[i,1:(ncol(hostsCombinations)/2)])
      rightSide <- as.numeric(hostsCombinations[i,(ncol(hostsCombinations)/2+1):ncol(hostsCombinations)])
      
      if ( all(SPhosts[leftSide,] <= Phosts[rightSide, c("cpu", "mem", "str")], TRUE) ) {
        
        priceAux <- vector()
        lastProvider <- 0
        penalty <- 1.1
        
        for ( j in 1:length(rightSide) ) {
          
          actualProvider <- Phosts[rightSide[j], "providerID"]
          arg1 <- Phosts[rightSide[j],"cpu"]
          arg2 <- Phosts[rightSide[j],"mem"]
          arg3 <- Phosts[rightSide[j],"str"]
          
          if (lastProvider == actualProvider) {
            priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "hosts")*24*penalty)
            penalty <- penalty + 0.1
          }
          else {
            priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "hosts")*24 )
          }
          
          lastProvider <- actualProvider
          
        }
        
        if ( all(priceAux <= pricePerDay, TRUE) ) {
          price <- c( price, sum(priceAux) )
          auxDF <- rbind(auxDF, hostsCombinations[i,])
        }
        
      }
      
    }
    
  }
  if (pricingType == "prized") {
    
    for (i in 1:nrow(hostsCombinations)) {
      
      leftSide <- as.numeric(hostsCombinations[i,1:(ncol(hostsCombinations)/2)])
      rightSide <- as.numeric(hostsCombinations[i,(ncol(hostsCombinations)/2+1):ncol(hostsCombinations)])
      
      if ( all(SPhosts[leftSide,] <= Phosts[rightSide, c("cpu", "mem", "str")], TRUE) ) {
        
        priceAux <- vector()
        lastProvider <- 0
        reward <- 0.9
        
        for ( j in 1:length(rightSide) ) {
          
          actualProvider <- Phosts[rightSide[j], "providerID"]
          arg1 <- Phosts[rightSide[j],"cpu"]
          arg2 <- Phosts[rightSide[j],"mem"]
          arg3 <- Phosts[rightSide[j],"str"]
          
          if (lastProvider == actualProvider) {
            priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "hosts")*24*reward)
            
            if (reward > 0.5) {
              reward <- reward - 0.1  
            }
            
          }
          else {
            priceAux <- c( priceAux, pricing(arg1, arg2, arg3, "hosts")*24 )
          }
          
          lastProvider <- actualProvider
          
        }
        
        if ( all(priceAux <= pricePerDay, TRUE) ) {
          price <- c( price, sum(priceAux) )
          auxDF <- rbind(auxDF, hostsCombinations[i,])
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