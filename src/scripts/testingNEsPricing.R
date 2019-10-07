for ( i in c(1, 2, 3, 4) ) {
  
  for ( j in c(6, 12, 24, 48) ) {
    
    for ( k in c(1, 2, 3, 4) ) {
      
      print(pricing(i, j, k, "nes"))
      
    }
    
  }
  
}