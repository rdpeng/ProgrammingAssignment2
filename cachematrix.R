## A pair of functions that cache the inverse of a matrix

## makeCacheMatrix creates matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(inverse) {
    i <<- inverse
  }
  getinverse <- function() {
    i
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function evaluates the inverse of the 
## matrix given by makeCacheMatrix 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if (!is.null(m)) {
    message("retreiving cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %% data
  x$setinverse(m)
  m
}
