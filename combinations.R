combinations <- function(provider, resource, resourceType, numberOfCombinations, allPossibilities) {

	i <- resourceType
	# count <- nrow(allPossibilities$ID)
	# # allPossibilities <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("ID", "providerID", "resourceID", "arg1", "arg2", "arg3"))
	# allPossibilities <- data.frame(matrix(ncol = (2 * numberOfCombinations + 1), nrow = 0))

	for (k in 1:numberOfProviders) {

		for (m in 1:length(providers[[k]][[i]])) {

			if (provider == k & resource == m) {
				next
			}
			else{
				allPossibilities <- rbind(allPossibilities, data.frame(P1=provider, R1=resource, P2=k, R2=m))
				# print(paste("P", provider, "R", resource, "P", k, "R", m))
			}

		}

	} 

	allPossibilities

}