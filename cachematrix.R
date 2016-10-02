## cachematrix.R contains following functions that
## enable us to calculate inverse of a matrix and cache it
## in the global environment so that it's value can be utilised.
## for further computations.

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        getInverse <- function() inverse
        setInverse <- function(z) {
                inverse <<- z
        }
        list(set = set , get = get , getInverse = getInverse , setInverse = setInverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##  If the inverse has already been calculated , then cacheSolve should retrieve 
##  the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        ## Checking if inverse is already calculated.
        inverse <- x$getInverse
        if(!is.null(inverse)){
                message("Getting cached inverse ........")
                return(inverse)
        }
        
        if(ncol(x) == nrow(x)){
                inverse <- solve(x)
                x$setInverse(inverse)
        }
        return(inverse)
}
