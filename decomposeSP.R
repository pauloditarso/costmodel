decomposeSP <- function(SP, resource) {

	SPaux <- data.frame(matrix(ncol=3, nrow=0))
	
	if ( length(SP[[paste(resource)]][[1]]) == 3 ) {

		for (i in 1:length(SP[[paste(resource)]])) { 
			SPaux <- rbind(SPaux, SP[[paste(resource)]][[i]]) 
		}

	}

	if (resource == "hosts") { SPaux <- setNames(SPaux, c("cpu", "mem", "str")) }
	if (resource == "links") { SPaux <- setNames(SPaux, c("cap", "del", "jit")) }
	if (resource == "nes") { SPaux <- setNames(SPaux, c("cap", "por", "que")) }
	
	SPaux
	
}