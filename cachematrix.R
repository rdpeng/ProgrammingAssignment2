## makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.
## cacheSolve      - This function computes the inverse of the special "matrix" returned by makeCacheMatrix 

## Returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    print(y)
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Returns the inverse

cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
