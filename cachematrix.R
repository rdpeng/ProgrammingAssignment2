## Put comments here that give an overall description of what your
## functions do

## The following functions take an invertible square matrix and
## cache the inverse of the given matrix.

## Write a short comment describing this function

##Function makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
    inv <- NULL              ## initialize inv as NULL
    set <- function(y) {
        x <<- y              ## value of matrix in parent environment
        inv <<- NULL         ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Write a short comment describing this function

## This function computes the inverse of the matrix returned by makeCacheMatrix function above.
## If the inverse has already been calculated cacheSolve just retrieves the data from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse data")
        return(inv)
    }
    data <- x$get()
        
    inv <- solve(data, ...)
        
    x$setinverse(inv)
        
    inv
}
