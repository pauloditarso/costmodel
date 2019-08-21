decomposeProv <- function(providers, resource) {

	auxProv <- data.frame(matrix(ncol=4, nrow=0))

	for (i in 1:length(providers)) {

		if ( length(providers[[i]][[paste(resource)]][[1]]) == 3 ) {

			for ( j in 1:length(providers[[i]][[paste(resource)]]) ) {

				arg1 <- as.numeric(providers[[i]][[paste(resource)]][[j]][1])
				arg2 <- as.numeric(providers[[i]][[paste(resource)]][[j]][2])
				arg3 <- as.numeric(providers[[i]][[paste(resource)]][[j]][3])

				# auxProv <- rbind( auxProv, data.frame(providerID=i, resourceType=paste(resource), resourceID=j, arg1=arg1, arg2=arg2, arg3=arg3) )
				auxProv <- rbind( auxProv, data.frame(providerID=i, resourceID=j, arg1=arg1, arg2=arg2, arg3=arg3) )
				
			}

		}

	} 

	auxProv

}