## Functions makeCacheMatrix and cacheSolve work together to 
## enable the cacheing of inverse matrices, in order to
## efficiently store these data objects for future reuse and 
## optimize usage of processing resources.

## makeCacheMatrix a form of the matrix x, that can be cached
## Function setsolve() saves the inverse of the matrix in the parent environment
## Function getsolve() returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseM <- NULL
        set <- function(y){
                x <<- y
                inverseM <<- NULL
                
        }
        get <- function() x
        setsolve <- function(solve) inverseM <<- solve
        getsolve <- function() inverseM
        list(set = set, get = get, setsolve = setsolve, 
             getsolve = getsolve)
        
}


## cacheSolve takes the cacheable form of the matrix, checks if the inverse of 
## the matrix has been cached, and returns the inverse;  if not, generates the
## inverse of the matrix, caches it and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseM <- x$getsolve() 
        if(!is.null(inverseM)) {
                message("getting cached data")
                return(inverseM)
        }        
        data <- x$get()
        inverseM <- solve(data, ...)
        x$setsolve(inverseM)
        inverseM           
}
