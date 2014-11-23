## makeCacheMatrix is a user defined datatype that maintains a cache and 
## provides functions to get and set the cached value.

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


## Function cacheSolve returns inverse of a matrix. It checks if the matrix invesrse is avalable 
## in the cache, if so it returns the cached value. If not, it creates a inverse, stores in the cache 
## and returns the results.
## cacheSolve takes variable of type makeCacheMatrix as its input. 

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
  message("Checking")
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
