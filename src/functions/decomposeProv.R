decomposeProv <- function(providers, resource, minResource) {

	auxProv <- data.frame(matrix(ncol=6, nrow=0))

	for (i in 1:length(providers)) {

		if ( length(providers[[i]][[paste(resource)]][[1]]) == 3 ) {

			for ( j in 1:length(providers[[i]][[paste(resource)]]) ) {

				arg1 <- as.numeric(providers[[i]][[paste(resource)]][[j]][1])
				arg2 <- as.numeric(providers[[i]][[paste(resource)]][[j]][2])
				arg3 <- as.numeric(providers[[i]][[paste(resource)]][[j]][3])
				auxPrice <- pricing(arg1, arg2, arg3, paste(resource))

				if ( arg1 >= as.numeric(minResource[1]) && arg2 >= as.numeric(minResource[2])
				     && arg3 >= as.numeric(minResource[3]) ) {
				  auxProv <- rbind( auxProv, data.frame(providerID=i, resourceID=j, arg1=arg1,
				                                        arg2=arg2, arg3=arg3, price=auxPrice) )
				}
				
			}

		}

	} 

	if (resource == "hosts") { auxProv <- setNames(auxProv, c("providerID", "resourceID", "cpu", "mem", "str", "price")) }
	if (resource == "links") { auxProv <- setNames(auxProv, c("providerID", "resourceID", "cap", "del", "jit", "price")) }
	if (resource == "nes") { auxProv <- setNames(auxProv, c("providerID", "resourceID", "cap", "por", "que", "price")) }
	
	auxProv

}