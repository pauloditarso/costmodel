resultsComputation <- function(resource) {
  
  if (resource == "hosts") {
    
    totalCols <- (ncol(hostsDF)-1)
    halfCols <- totalCols/2
    
    requestSide <- 0
    answersSide <- 0
    namesList <- vector()
    
    auxDF <- data.frame()
    
    for ( i in 1:nrow(hostsDF) ) {
      
      requestSide <- as.numeric(hostsDF[i,1:halfCols])
      answersSide <- as.numeric(hostsDF[i,(halfCols+1):totalCols])
      price <- hostsDF[i,"price"]
      auxReq <- list()
      auxAns <- list()
      
      for ( j in requestSide ) {
        auxReq <- append(auxReq, as.integer(SPhosts[j,]))
      }
      
      for ( k in answersSide ) {
        auxAns <- append(auxAns, as.integer(Phosts[k,]))
      }
      
      auxDF <- rbind(auxDF, c(auxReq, auxAns, price))
      
    }
    
    for ( l in 1:length(requestSide) ) {
      namesList <- c( namesList, paste("r", l,".cpu", sep = ""), paste("r", l,".mem", sep = ""), 
                      paste("r", l,".str", sep = "") )
    }
    for ( m in 1:length(answersSide) ) {
      namesList <- c( namesList, paste("a", m,".pID", sep = ""), paste("a", m,".rID", sep = ""), 
                      paste("a", m,".cpu", sep = ""), paste("a", m,".mem", sep = ""), paste("a", m,".str", sep = "") )
    }
    
    namesList <- c(namesList, "price")
    colnames(auxDF) <- namesList
    return(auxDF)  
    
  }
  
  if (resource == "links") {
    
    totalCols <- (ncol(linksDF)-1)
    halfCols <- totalCols/2
    
    requestSide <- 0
    answersSide <- 0
    namesList <- vector()
    
    auxDF <- data.frame()
    
    for ( i in 1:nrow(linksDF) ) {
      
      requestSide <- as.numeric(linksDF[i,1:halfCols])
      answersSide <- as.numeric(linksDF[i,(halfCols+1):totalCols])
      price <- linksDF[i,"price"]
      auxReq <- list()
      auxAns <- list()
      
      for ( j in requestSide ) {
        auxReq <- append(auxReq, as.integer(SPlinks[j,]))
      }
      
      for ( k in answersSide ) {
        auxAns <- append(auxAns, as.integer(Plinks[k,]))
      }
      
      auxDF <- rbind(auxDF, c(auxReq, auxAns, price))
      
    }
    
    for ( l in 1:length(requestSide) ) {
      namesList <- c( namesList, paste("r", l,".cap", sep = ""), paste("r", l,".del", sep = ""), 
                      paste("r", l,".jit", sep = "") )
    }
    for ( m in 1:length(answersSide) ) {
      namesList <- c( namesList, paste("a", m,".pID", sep = ""), paste("a", m,".rID", sep = ""), 
                      paste("a", m,".cap", sep = ""), paste("a", m,".del", sep = ""), paste("a", m,".jit", sep = "") )
    }
    
    namesList <- c(namesList, "price")
    colnames(auxDF) <- namesList
    return(auxDF)  
    
  }
  
  if (resource == "nes") {
    
    totalCols <- (ncol(nesDF)-1)
    halfCols <- totalCols/2
    
    requestSide <- 0
    answersSide <- 0
    namesList <- vector()
    
    auxDF <- data.frame()
    
    for ( i in 1:nrow(nesDF) ) {
      
      requestSide <- as.numeric(nesDF[i,1:halfCols])
      answersSide <- as.numeric(nesDF[i,(halfCols+1):totalCols])
      price <- nesDF[i,"price"]
      auxReq <- list()
      auxAns <- list()
      
      for ( j in requestSide ) {
        auxReq <- append(auxReq, as.integer(SPnes[j,]))
      }
      
      for ( k in answersSide ) {
        auxAns <- append(auxAns, as.integer(Pnes[k,]))
      }
      
      auxDF <- rbind(auxDF, c(auxReq, auxAns, price))
      
    }
    
    for ( l in 1:length(requestSide) ) {
      namesList <- c( namesList, paste("r", l,".cap", sep = ""), paste("r", l,".por", sep = ""), 
                      paste("r", l,".que", sep = "") )
    }
    for ( m in 1:length(answersSide) ) {
      namesList <- c( namesList, paste("a", m,".pID", sep = ""), paste("a", m,".rID", sep = ""), 
                      paste("a", m,".cap", sep = ""), paste("a", m,".por", sep = ""), paste("a", m,".que", sep = "") )
    }
    
    namesList <- c(namesList, "price")
    colnames(auxDF) <- namesList
    return(auxDF)  
    
  }
  
  if ( resource != "hosts" & resource != "links" & resource != "nes" ) {
    print("ERROR: invalid type of resource!!!")
  }
  
  
}