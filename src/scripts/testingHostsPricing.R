for ( i in c(1, 2, 4, 8, 16, 32) ) {
  
  for ( j in c(4, 8, 16, 32, 64, 128) ) {
    
    for ( k in c(128, 256, 512, 1024, 2048, 4096) ) {
      
      print(pricing(i, j, k, "hosts"))
      
    }
    
  }
  
}