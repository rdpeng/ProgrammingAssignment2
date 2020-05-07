# Caching Inverse of a Matrix

# Creates a matrix that can cache it's inverse
# Args:
#   x: A matrix (Optional)
# Returns:
#   A matrix with functions to get/set value & get/set inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

# Computes the inverse of a matrix. If the inverse has already been calculated before, the cached inverse is returned.
# Args:
#   x: A matrix
#   ...: Extra arguments
# Returns:
#   The inverse of the matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
