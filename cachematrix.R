## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache  its inverse.

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


## Write a short comment describing this function
##If the inverse has been already been calculated ( and the matrix has not changed),then its should
##retrieve the inverse  from the cache.

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
