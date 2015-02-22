## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makes a list based on a matrix that can use cacheSolve to cache matrix inverse
## ginv() is less error prone than solve()
## ginv() would also have allowed non-square matricies
## ginv() requires library("MASS")
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
## ginv() is less error prone than solve()
## ginv() would also have allowed non-square matricies
## ginv() requires library("MASS")
  setinv <- function(inv) m <<- solve(x)
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
# uses solve to compute inverse of some square matricies
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached matrix inverse")
    return(m)
  }
  data <- x$get()
## ginv() is less error prone than solve()
## ginv() would also have allowed non-square matricies
## ginv() requires library("MASS")
  m <- solve(data, ...)
  x$setinv(m)
  m
}
