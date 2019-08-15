## Script to cache the inverse of a matrix

## function creates a list of functions to cache a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Function to compute the inverse of a matric and cache it if it hasn't been cached previously

cacheSolve <- function(x, ...) {
  inv1 <- x$getinv()
  if(!is.null(inv1)) {
    message("getting cached data")
    return(inv1)
  }
  data <- x$get()
  inv1 <- solve(data, ...)
  x$setinv(inv1)
  inv1
}
