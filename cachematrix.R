## Usage:	funcList = makeCacheMatrix( x )
## 	cacheSolve( funcList )

## Constructor: returns a list of 4 functions: set, get, setSove, getSolve

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(y) {			# set method
		x <<- y
		s <<- NULL			# s is NOT local
	}
	get <- function() x			# get method
	setSolve <- function(solve)  s <<- solve	# setSove method, returns solve
	getSolve <- function() s		# getSolve method
	list( set = set, get = get,		# returns the list of 4 functions
	setSolve = setSolve,
	getSolve = getSolve )
}


## Return a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        s <- x$getSolve()			# get inverse matrix from the cache
	if( !is.null(s) ) {
		message( "getting data from cache" )
		return( s )			# cache data returned
	}
	message( "new calculation" )
	data = x$get()			# data = matrix
	s <- solve(data,...)
	x$setSolve(s)			# calculate & put to cache
	s				# return inverse matrix
}
