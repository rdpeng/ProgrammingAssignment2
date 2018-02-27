## the first funcion creates an special matrix that can set the value of the vector, get the actual value, and
## does the same with the inverse of the matrix, the second calculates the inverse or gets it from the matrix if the inverse already exists

## this function defines can set the value of the matrix x or retrive them if it exists, it can do the same with the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculate the inverse of the matrix x or retreive it if already exists

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if ( !is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
