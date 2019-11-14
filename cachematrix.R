## This functions cache and compute the inverse of a matrix

## This function creates a special "matrix" that can cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inversa <<- inv
  getinv <- function() inversa
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse of the special "matrix" created with the above function. 
## it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache

cacheSolve <- function(x, ...) {
  inversa <- x$getinv()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...)
  x$setinv(inversa)
  inversa
}
