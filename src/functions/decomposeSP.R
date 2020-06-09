decomposeSP <- function(SP, resource) {

	SPaux <- data.frame(matrix(ncol=4, nrow=0))
	
	if ( length(SP[[paste(resource)]][[1]]) == 3 ) {

		for (i in 1:length(SP[[paste(resource)]])) {
		  aux <- SP[[paste(resource)]][[i]]
		  auxCost <- pricing(aux[1], aux[2], aux[3], paste(resource))
		  aux <- c(aux, auxCost)
			SPaux <- rbind(SPaux, aux) 
		}

	}

	if (resource == "hosts") { SPaux <- setNames(SPaux, c("cpu", "mem", "str", "cost")) }
	if (resource == "links") { SPaux <- setNames(SPaux, c("cap", "del", "jit", "cost")) }
	if (resource == "nes") { SPaux <- setNames(SPaux, c("cap", "por", "que", "cost")) }
	
	SPaux
	
}