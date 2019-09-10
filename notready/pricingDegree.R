pricingDegree <- function(lowerlimit, upperlimit, interval, data) {
  
  # ((c(1:300)%/%8)%%3)%%2
  
	for (i in 1:interval) {
		if( (0 <= data) & (data < 6) ) { print( (upperlimit+lowerlimit)/2 ) }
		if( (6 <= data) & (data < 12) ) { print( lowerlimit ) }
		if( (12 <= data) & (data < 18) ) { print( upperlimit ) }
		if( (18 <= data) & (data < 24) ) { print( (upperlimit+lowerlimit)/2 ) }
		if( (0 > data) & (data > 24) ) { return(0) }
	}
}
