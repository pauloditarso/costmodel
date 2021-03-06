findNEs <- function(SPnes) {

	if ( as.numeric(SPnes[[1]][1]) != 0 ) {

		answersDF <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), c("matchID", "spID", "req1", "req2", "req3", "providerID", "resourceID", "arg1", "arg2", "arg3"))
		matchID <- 0
		spID <- 0
		req1 <- req2 <- req3 <- 0
		providerID <- 0
		resourceID <- 0
		arg1 <- arg2 <- arg3 <- 0
		finished <- FALSE
		matched <- FALSE

		while(!finished) {

			matchID <- matchID + 1

			for ( i in 1:length(SPnes) ) {

				spID <- i
				req1 <- as.numeric(SPnes[[i]][1])
				req2 <- as.numeric(SPnes[[i]][2])
				req3 <- as.numeric(SPnes[[i]][3])

				for ( j in 1:numberOfProviders ) {

					providerID <- j
					name <- paste("p", j, sep = "")
					auxProvider <- providers[[name]]

					for ( k in 1:length(auxProvider$nes) ) {

						resourceID <- k
						arg1 <- as.numeric(auxProvider$nes[[k]][1])
						arg2 <- as.numeric(auxProvider$nes[[k]][2])
						arg3 <- as.numeric(auxProvider$nes[[k]][3])

						colnames(answersDF) <- c("matchID", "spID", "req1", "req2", "req3", "providerID", "resourceID", "arg1", "arg2", "arg3")
						if ( nrow(answersDF[answersDF$matchID == matchID & answersDF$spID == i,]) == 0 & nrow(answersDF[answersDF$providerID == j & answersDF$resourceID == k,]) == 0 ) {

							if ( all((SPnes[[i]] <= auxProvider$nes[[k]]), na.rm = TRUE) ) {
								answersDF <- rbind( answersDF, c("matchID"=matchID, "spID"=spID, "req1"=req1, "req2"=req2, "req3"=req3, "providerID"=providerID, "resourceID"=resourceID, "arg1"=arg1, "arg2"=arg2, "arg3"=arg3) )
							}	

						}

						########################if ( i == length(SPnes) & j == numberOfProviders & )

					}

				}

			}

			if ( answersDF$spID[length(answersDF$spID)] < length(SPnes) ) {

				answersDF <- answersDF[-c(as.numeric(row.names(answersDF[answersDF$matchID == matchID,]))),]
				finished <- TRUE

			}

		}

		print(answersDF)

	}
	else {
		return(-1)
	}

}