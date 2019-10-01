matching <- function(SP, providers) {

	# for loop to analyse each request resource
	for (i in c("hosts", "links", "nes")) {

		# for loop to analyse each request resource number
		for (j in 1:length(SP[[i]])) {

			if ( length(SP[[i]][[j]]) == 1 ) next

			# each request formulated
			requested <- paste(unlist(SP[[i]][[j]]), collapse=' ')
			print(paste("sp", i, "#", j, requested))
			
			# for loop to analyse each provider
			for (k in 1:numberOfProviders) {
			
				for (m in 1:length(providers[[k]][[i]])) {

					if ( all((SP[[i]][[j]] <= providers[[k]][[i]][[m]]), na.rm = TRUE) ) {

						properties <- paste(unlist(providers[[k]][[i]][[m]]), collapse=' ')
						print(paste("p", k, i, "#", m, properties))
						break
					}
					# else {
					# 	next
					# }
					
				}

			}			
		}

	}
}
	