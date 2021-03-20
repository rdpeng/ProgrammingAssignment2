## Matrix inversion is usually a costly computation so in order to save time it is beneficial to cache the inverse. In doing so the same computation
## need not be continuously repeated and time is saved. This is a pair of functions that cache the inverse of a matrix.

## This function creates a special list that stores and sets matrices and their inverses.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes the matrix computed by makeCacheMatrix as an argument and prints the inverse of the matrix that was inputted as the argument of 
## makeCacheMatrix. If the inverse has already been cached, it will print the cached inverse. If the inverse has not yet been calculated, it will 
## compute the inverse and cache it for future caching.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setinv(i)
  i
}
