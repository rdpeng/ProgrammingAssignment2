## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix<- function(x = matrix()) {
    inverse_result <- NULL
    set <- function(y) {
        x <<- y
        inverse_result <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_result <<- inverse
    getinverse <- function() inverse_result
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inverse_result <- x$getinverse()
    if(!is.null(inverse_result)) {
        message("getting cached data")
        return(inverse_result)
    }
    data <- x$get()
    inverse_result <- solve(data,...)
    x$setinverse(inverse_result)
    inverse_result
}

