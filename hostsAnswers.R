hostsAnswers <- function(hostsCombinations, pricePerDay) {

  auxDF <- data.frame()
  price <- vector()
  
#  if ( !exists("providersFactor", envir = .GlobalEnv) ) providersFactor <- 0.0005
  
  for (i in 1:nrow(hostsCombinations)) {
    
    print(i)
    leftSide <- as.numeric(hostsCombinations[i,1:(ncol(hostsCombinations)/2)])
    rightSide <- as.numeric(hostsCombinations[i,(ncol(hostsCombinations)/2+1):ncol(hostsCombinations)])
    
    if ( all(SPhosts[leftSide,] <= Phosts[rightSide, c("cpu", "mem", "str")], TRUE) ) {
      
      priceAux <- 0
      arg1 <- Phosts[rightSide,"cpu"][[1]]
      arg2 <- Phosts[rightSide,"mem"][[1]]
      arg3 <- Phosts[rightSide,"str"][[1]]
      priceAux <- pricing(arg1, arg2, arg3, "hosts")*24 
      
      if ( priceAux <= pricePerDay ) {
        price <- c(price, priceAux)
        auxDF <- rbind(auxDF, hostsCombinations[i,])
      }

    }
    
  }
   
  auxDF <- cbind(auxDF, price)
  auxDF

}