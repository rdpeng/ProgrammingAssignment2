## makeCacheMatrix is a function that wraps a matrix
## with methods to calculate its inverse and cache the results

## makeCacheMatrix wraps a matrix with cache capabilities

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## cacheSolve make use of a matrix created with makeCacheMatrix
## to use the caching capabilities provided by it to save from
## repeating the intensive solve function 

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}
