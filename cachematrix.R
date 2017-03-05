## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## The inverse is initialized to a zero matrix, and the sum of all matrix elements
## is tested in the function cacheSolve.
## The inverse of a square matrix with any number of columns will be computed. 
makeCacheMatrix <- function(xmatrix = matrix(numeric(0), 0,0)) {
        inverse <- matrix(numeric(0), 0,0)
        
        set <- function(y) {
                xmatrix <<- y
                inverse <<- matrix(numeric(0), 0,0)
        }
        get <- function() xmatrix
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the function cachesolve retrieves the inverse
## from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!sum(inverse) == 0) {
                message("getting cached data")
                return(inverse)
        } else {
                matrixdata <- x$get()
                inverse <- solve(matrixdata, ...)
                x$setinverse(inverse)
                inverse}
}
