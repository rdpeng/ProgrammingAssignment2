## Within  the next 2 functions you can  cache the inverse matrix making it 
## simpler and saving time and effort on the computer by not computing  the 
## same result of an already calculated value

## Creates a matrix object that can cache the inverse of  it 

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The next function computes the inverse matrix created in the previous 
## function (makeCacheMatrix). In case the inverse has been already calculted
## and the matrix remains unchanged, then the function will retrive the inverse
## from the cache

cacheSolve <-function(m, ...) {
    i <- m$getinv()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  mat <- m$get()
  i <- solve(mat, ...)
  m$setinv(i)
  i
}