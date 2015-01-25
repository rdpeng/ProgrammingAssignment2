## makeCacheMatrix is a function to build a special vector which actually is a list of functions
## set is a function to set value of the matrix
## get is a function to get value of the matrix
## setsolve is a function to give the value of 'solve' to 'm' so that we can calculate the inverse in next function.
## getsolve is a function to get the solve()

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
