## The following are a pair of functions that cache 
## the inverse of a matrix. Caching the inverse of
## a martix poses an advantage because costly or 
## repeated computation can be avoided.

## The makeCacheMatrix function creates a "special"
## matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is inverse of 'x'.
  inv_x <- x$getinverse()
  if (!is.null(inv_x)) {
    message("getting cached inverse matrix")
    return(inv_x)
  } else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}
