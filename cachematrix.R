## Caching the inverse of a matrix
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than to compute
## it repeatedly. Below are a pair of functions that are used to create
## a spcial object that stores a matrix and cache its inverse.

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<-y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## This function will compute the inverse of the special matrix created by
## makeCacheMatrix above. If the inverse has already been calculated then it should 
## be retrieved from cache.

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

