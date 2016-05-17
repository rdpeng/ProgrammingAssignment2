
## Used together these two functions create a cache for a matrix's inverse.
## Developer: Hannah Hsu
## Developed on: 05/16/2016

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y){
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setInverse <- function(inverse) inv <<- inverse
            getInverse <- function() inv
            list(set=set, get=get, 
                 setInverse = setInverse,
                 getInverse = getInverse)
}


## This 2nd function checks to see if the inverse of the original matrix has 
## already been calculated. If so, it returns the inverse from the cache
## and skips any further computation. Otherwise it calculates the inverse of
## the matrix and sets the inverse in the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)){
          message("getting cached data")
          return(inv)
        }
        mat <- x$get()
        inv <- solve(mat,...)
        x$setInverse(inv)
        inv
}
