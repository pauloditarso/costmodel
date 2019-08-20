decomposeSP <- function(SP, resource) {

	SPaux <- data.frame()

	for (i in 1:length(SP[[paste(resource)]])) { SPaux <- rbind(SPaux, SP$hosts[[i]]) }

	SPaux
	
}