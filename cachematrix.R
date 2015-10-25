
makeCacheMatrix <- function(x = matrix()) {
  ##stores list named makeCacheMatrix with matrix, pull from other env., inverse, pull from other env.
  I <- NULL
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setI <- function(inverse) I <<- inverse
  getI <- function() I
  list(set=set, get=get, setI=setI, getI=getI)
}


cacheSolve <- function(x, ...) {
  ##pulls from other environment or returns a matrix that is the inverse of 'x'
  I <- x$getI()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data)
  x$setI(I)
  I
}