## assignment is to write a pair of functions that cache
## the inverse of a matrix

## creates a special matrix object that can cache
## its own matrix
## set the value for the vector
## get the value for the vector
## set the value for the inverse
## get the value for the inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## computes the inverse of the matrix above
## if the inverse has already been calculated and 
## the matrix has not changed, the cachesolve should 
## retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
