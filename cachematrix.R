## makeCacheMatrix is a function that creates a special matrix that can cache its inverse and cachesolve
## computes the inverse given by makeCacheMatrix so it does not have to be calculated again.

## makeCacheMatrix: Function that creates a special matrix object where the inverse of it is cached

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##cachesolve:Function that computes the inverse of the special matrix in returned by the makeCacheMatrix. 
##If the inverse has already been calculated, it should retrieve the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
