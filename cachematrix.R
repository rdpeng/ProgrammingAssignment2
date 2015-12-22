# Matrix Inversion is a computationally expensive operation.Here is an 
# arrangement to store the inverse of a matrix into a Cache which can be 
# retrieved whenever we intend to perform the same operation for the a
# particular Matrix.

# This function creates a list to get & set a matrix 
# and to get & set it's inverse into the cache.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve()
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


# This function checkes for the inverse of a matrix in the cache and 
# retieves it if present,else computes it.


cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
