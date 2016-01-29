## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

      Inv <- NULL
      set <- function(y) {
            x <<- y
            Inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) Inv <<- inverse
      getInverse <- function() Inv
      list(set = set,
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {

              ## Return a matrix that is the inverse of 'x'
      Inv <- x$getInverse()
      if (!is.null(Inv)) {
            message("getting cached data")
            return(Inv)
      }
      mat <- x$get()
      Inv <- solve(mat, ...)
      x$setInverse(Inv)
      Inv
}
      
