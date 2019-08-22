#combinacoes <- data.frame()
for (i in 1:ncol(indexes)) {
  for (j in 1:nrow(indexes)) {
    
    request <- j
    resources <- indexes[,i]
    
    if ( all(SPnes[request,] <= Pnes[resource, c("cap", "por", "que")]) == TRUE ) {
      print(paste("SPnes", request, "Pnes", resource))  
    }
    
  }
}
rm(i,j,request,resource)