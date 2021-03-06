pricing <- function(arg1, arg2, arg3, resource) {
  
  price <- 0
  auxVector <- c(arg1, arg2, arg3)
  
  if ( resource == "hosts" ) price <- sum(auxVector/referenceHost[1:3])/3 * referenceHost[4]
  if ( resource == "links" ) price <- sum(auxVector/referenceLink[1:3])/3 * referenceLink[4]
  if ( resource == "nes" ) price <- sum(auxVector/referenceNE[1:3])/3 * referenceNE[4]
  
  return(price)
  
}