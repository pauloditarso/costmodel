arg1 <- c(3,2,4,4,4,4,2,15,4,4,8)
arg2 <- c(8,8,8,11,2,1,1,5,5,6,9)

soma <- 0
for (i in 1:length(arg1)) {
  soma <- soma + Phosts[Phosts$providerID == arg1[i],][arg2[i],"price"]
}

print(soma)