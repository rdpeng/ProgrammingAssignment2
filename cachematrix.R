## These pair of functions cache the inverse of a matrix!!


## makeCacheMatrix creates a special "matrix" object that 
## can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

      invm <- NULL
      set <- function(y) {
            x <<- y
            invm <<- NULL
      }
      get <- function() x
      setinv <- function(solve) invm <<- solve
      getinv <- function() invm
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
      
}

## cacheSolve: This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above. If 
## the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the
## inverse from the cache.
cacheSolve <- function(x, ...) {
        
      invm <- x$getinv()
      if(!is.null(invm)) {
            message("getting cached data")
            return(invm)
      }
      data <- x$get()
      invm <- solve(data, ...)
      x$setinv(invm)
      invm
}
