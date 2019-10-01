#combinacoes <- data.frame()
for (i in 1:ncol(indexes)) {
  for (j in 1:nrow(indexes)) {
    
    request <- j
    resources <- indexes[,i]
    
    if ( all(SPnes[request,] <= Pnes[resources, c("cap", "por", "que")]) == TRUE ) {
      print(paste("SPnes", request, "Pnes", resources))  
    }
    
  }
}
rm(i,j,request,resources)