
## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeteadly

## Below is a function that creates a special matrix object that can chache its inverse

makeMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
   setinverse <- function(inverse) inv <<- inverse
   getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function computes the inverse of the special matrix created by makeCacheMatrix. 
## If the inverse of a matrix has been calculated and the matrix itself has not been modified, 
## then it retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinverse(inv)
  inv
}
