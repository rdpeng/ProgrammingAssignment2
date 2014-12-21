makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setMatrix <- function(inv_matrix) inv <<- inv_matrix
  getMatrix <- function() inv
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

cacheSolve <- function(x, ...) {
  inv <- x$getMatrix()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setMatrix(inv)
  inv
}
