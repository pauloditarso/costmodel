findMatchResource <- function(request, resource) {

	i <- resource
	count <- 0
	#matchResourceDF <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("matchID", "providerID", "resourceID", "arg1", "arg2", "arg3"))
	matchResourceDF <- data.frame()

	for (k in 1:numberOfProviders) {		

		for (m in 1:length(providers[[k]][[i]])) {

			if ( nrow(hostsDF[hostsDF$providerID == 1 & hostsDF$resourceID == 7,]) > 0 ) next

			if ( all((request <= providers[[k]][[i]][[m]]), na.rm = TRUE) ) {
				
				count <- count + 1
				arg1 <- as.numeric(providers[[k]][[i]][[m]][1])
				arg2 <- as.numeric(providers[[k]][[i]][[m]][2])
				arg3 <- as.numeric(providers[[k]][[i]][[m]][3])
				matchResourceDF <- rbind(matchResourceDF, data.frame(matchID=count, providerID=k, resourceID=m, arg1=arg1, arg2=arg2, arg3=arg3))

			}
				
		}

	}			

	return(matchResourceDF)

}
