indexes <- data.frame()
for (i in 1:nrow(SPnes)) {
  for (j in 1:nrow(Pnes)) {
    for (k in 1:nrow(SPnes)) {
      if (i == k) next
      for (l in 1:nrow(Pnes)) {
        if(j == l) { next }
        if ( nrow( indexes[(indexes$k == i & indexes$l == j) & (indexes$i == k & indexes$j == l),] ) > 0 ) { next }
        indexes <- rbind(indexes, data.frame("i"=i, "j"=j, "k"=k, "l"=l))
      }
    }
  }
}
rm(i, j, k, l)