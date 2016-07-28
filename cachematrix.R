## Put comments here that give an overall description of what your
## functions do

## Function that creates a 'class' that stores the matrix and its inverse
## and provide access methods to them
makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  set <- function(y) {
    m <<- y
    inv <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that calculates the inverse of a cached matrix
## or returns the inverse if it was already calculated

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- m$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- m$get()
  inv <- solve(data)
  m$setinverse(inv)
  inv
}
