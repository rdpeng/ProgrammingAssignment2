makeCacheMatrix <- function(x = matrix()) {
  MatInverse<- NULL
  set <- function(y) {
    x <<- y
    MatInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) MatInverse <<- inverse
  getInverse <- function() MatInverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




cacheSolve <- function(x, ...) {
  MatInverse <- x$getInverse()
  if (!is.null(MatInverse)) {
    message("getting cached data")
    return(MatInverse)
  }
  Matrix <- x$get()
  MatInverse <- solve(Matrix, ...)
  x$setInverse(MatInverse)
  MatInverse
}