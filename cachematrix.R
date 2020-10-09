## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {                                ##set value of matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x                                 ##get value of matrix
  setinv <- function(inverse) inv <<- inverse                ##set value of inverse
  getinv <- function() inv                            ##get value of inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheInv <- function(x, ...) {                    ##checks cache data
  inv <- x$getinv()                               ##checks if inverse is null
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)                                   ##returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)                         ##calculates inverse value
  x$setinv(inv)
  inv
}
