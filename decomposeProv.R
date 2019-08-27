decomposeProv <- function(providers, resource) {

	auxProv <- data.frame(matrix(ncol=5, nrow=0))

	for (i in 1:length(providers)) {

		if ( length(providers[[i]][[paste(resource)]][[1]]) == 3 ) {

			for ( j in 1:length(providers[[i]][[paste(resource)]]) ) {

				arg1 <- as.numeric(providers[[i]][[paste(resource)]][[j]][1])
				arg2 <- as.numeric(providers[[i]][[paste(resource)]][[j]][2])
				arg3 <- as.numeric(providers[[i]][[paste(resource)]][[j]][3])

				auxProv <- rbind( auxProv, data.frame(providerID=i, resourceID=j, arg1=arg1, arg2=arg2, arg3=arg3) )
				
			}

		}

	} 

	if (resource == "hosts") { auxProv <- setNames(auxProv, c("providerID", "resourceID", "cpu", "mem", "str")) }
	if (resource == "links") { auxProv <- setNames(auxProv, c("providerID", "resourceID", "cap", "del", "jit")) }
	if (resource == "nes") { auxProv <- setNames(auxProv, c("providerID", "resourceID", "cap", "por", "que")) }
	
	auxProv

}