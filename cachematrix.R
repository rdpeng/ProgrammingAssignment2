
## demonstrating the concept of lexical scoping
## The function makeCacheMatrix  creates a special "matrix" object that can cache its inverse.
## Using caching the inverse of a matrix can be  rather than computing  repeatedly

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse){ inv <<- Inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## To compute the inverse of the special "matrix" created by makeCacheMatrix. 
## If the inverse has already been calculated, then retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)){## if exists retrieve the inverse from the cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv      
}
