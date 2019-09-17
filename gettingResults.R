auxDF <- data.frame()

for (i in 1:10) {
  
  auxDF <- rbind(auxDF, as.numeric(Rmisc::CI(get(paste("results_", i, sep="")))))
  
}

colnames(auxDF) <- c("upper", "mean", "lower")
print(auxDF)
#psych::error.bars(t(auxDF), eyes = FALSE)