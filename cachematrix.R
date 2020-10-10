## Inverting matrices is a relatively intensive computation, and if the inverse
## of the matrix doesnt change, it may be easier to simply cache the result.
## The following functions create a matrix then cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
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


## This function calculates the inverse of the previous matrix, or if it has already
## been calculated then this function retrieves it from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
