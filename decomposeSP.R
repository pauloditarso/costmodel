decomposeSP <- function(SP, resource) {

	SPaux <- data.frame()
	
	if ( length(SP[[paste(resource)]][[1]]) == 3 ) {

		for (i in 1:length(SP[[paste(resource)]])) { 
			SPaux <- rbind(SPaux, SP[[paste(resource)]][[i]]) 
		}

	}

	SPaux
	
}