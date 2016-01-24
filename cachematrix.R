## This is the second programming assignment (Lexical Scoping) 
## Cach and inverse functions

## Matrix object constructor
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      ## check if new value is the same then previous one
      if (is.null(m) || !identical(y,m)) { 
        m <<- y
        mi <<- NULL
      }
  }
  get <- function() m
  setinvert <- function(inverted) mi <<- inverted
  getinvert <- function() mi
  list(set = set, get = get,
       setinvert = setinvert,
       getinvert = getinvert)
}

## Invertion with cache
cacheSolve <- function(x, ...) {
  m <- x$getinvert()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  mi <- solve(data, ...)
  x$setinvert(mi)
  mi
}
