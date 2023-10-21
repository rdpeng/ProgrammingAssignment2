## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # var and functions
        matrizinversa <- NULL
        set <- function( y ){
                x <<- y
                matrizinversa <<- NULL
                }
                get <- function() ( x )
        setInversa <- function( calculoinversa ) ( matrizinversa <<- calculoinversa )
        getInversa <- function() ( matrizinversa )
        list( set = set, get = get, setInversa = setInversa, getInversa = getInversa )
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        solinversa <- x$getInversa()
        if (!is.null(solinversa)) {
                 message( "getting cached data" )
                 return( solinversa )
                 }
        data <- x$get()
        solinversa <- solve(data, ...)
        x$setInversa(solinversa)
        solinversa
}
