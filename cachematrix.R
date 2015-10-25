## These two functions allow a user to solve a matrix for it's inverse. To save on
## computation time, the inverse matrix is cached and returned if available.

## makeCacheMatrix is a function that solves a matrix by calculating it's inverse, then
## caches the inverse matrix

makeCacheMatrix <- function(x = matrix()) {   ##set x equal to the given matrix
  m <- NULL                                   ##set m as NULL, later we can check for a cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x                            ##get the value of the matrix
  setInverse <- function(solve) m <<- solve      ##set the inverse by using the Solve f
  getInverse <- function() m                     ##get the inverse, set it as M
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}                                 ##nothing is returned by this function, answer is cached as M


## cacheSolve returns the cached inverse matrix if it has been previously cached.
## If not it solves the matrix and sets the cached value to the inverted matrix

cacheSolve <- function(x, ...) {
  m <- x$getInverse()                   ##set M equal to the cached inverse matrix
  if(!is.null(m)) {                     ##If M is not NUll, tell the user that there 
    message("getting cached data")      ##is cached data, and return the cached value (M)
    return(m)
  }
  data <- x$get()                       ##set data equal to the original matrix
  m <- solve(data, ...)                 ##Use solve to get the inverse matrix
  x$setInverse(m)                       ##Set m equal to the inverse matrix
  m                                     ##Return m to the console
}