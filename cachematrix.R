## makeCacheMatrix creates a special "matrix" that can cache its inverse

## The function returns a list containing a function to set the matrix, get the matrix,
## set the inverse, and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      gerinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cachesolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cache data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
