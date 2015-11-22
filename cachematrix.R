##Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.


## The first function,  makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##1. set the matrix
##2. get the matrix
##3. set the inverse of the matrix
##4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it  gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache via the  setinverse  function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
