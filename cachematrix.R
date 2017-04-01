## caching the inverse of a matrix
## using the following functions

## creates special matrix object that cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set<- function(y){
                x <<- NULL
                inv <<- NULL
                }
        get <- function() x
                setInverse <- function(inverse) inv <<- inverse
                        getInverse <- function() inv
                                list(set = set,
                                     get = get,
                                     setInverse = setInverse,
                                     getInverse = getInverse)

}


## this function computes the inverse of the special matrix created by
## makeCacheMatrix above

cacheSolve <- function(x, ...) {
      ## return a matrix that is inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
                }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
        
               }
