## These functions take a matrix, calculate its inverse, and then store the inverse so it can be called without 
## recalculating it each time.

## This function creates a list to set the value of a matrix and its inverse, and then also get the value of 
## that matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  q <- NULL
  set <- function(y) {
    x <<- y
    q <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) q <<- solve
  getinverse <- function() q
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function calculates the value of the inverse of the matrix, but first checks to see if the inverse has
## already been calculated, and if so, gets the value from the cache.  If the inverse has not already been cached,
## it calculates the value. This function returns the inverse of the matrix, with a message stating the value came
## from the cache if that's where it was pulled from.

cacheSolve <- function(x, ...) {
  q <- x$getinverse()
  if(!is.null(q)) {
    message("getting cached data")
    return(q)
  }
  data <- x$getinverse()
  q <- solve(data, ...)
  x$setinverse(q)
  q
}

