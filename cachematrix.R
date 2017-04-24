TEST


## Assignment: Caching the Inverse of a Matrix

## 1. makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
          x <<- y
          inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse, getinverse = getinverse)
}


## 2. cacheSolve: This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
      }
      raw.mat <- x$get()
      inv <- solve(raw.mat)
      x$setinverse(inv)
      inv
  ## Return a matrix that is the inverse of 'x'
}
