decomposeProv <- function(providers, resource) {

	

	auxProv <- data.frame()

	for (i in 1:numberOfProviders) {

		for ( j in 1:length(providers[[i]][[paste(resource)]]) ) {

			# auxProv <- rbind(auxProv, providers[[i]][[paste(resource)]][j])
			print(paste("i", i, "j", j))
		}

	} 

	# auxProv

}