
## These two function, when called in conjuncture, will either calculate
## the inverse of a matrix, or, if already calculated, call the inverse
## matrix from the cache


## This first function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  setM <- function(y) {
    x <<- y
    m <<- NULL
  }
  getM <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(setM = setM, 
       getM = getM,
       setinv = setinv,
       getinv = getinv)

}

## This second function computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated, then the cacheSolve should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
  
  m<- x$getinv
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
