## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.

## The functions below cache the inverse of a matrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

## example:
# x <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

# first run:
  # makeCacheMatrix.result <- makeCacheMatrix(x)
  # cacheSolve(makeCacheMatrix.result)

# second run:
  # cacheSolve(makeCacheMatrix.result)
  # will state 'getting cached data of matrix ...'



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data of matrix ...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv   
}
