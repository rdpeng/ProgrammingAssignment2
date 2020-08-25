## This function caches a list of objects, including the inverse of a matrix.
## Since we are supplied with a matrix (and not creating one) the calling function coerces the matrix to an array.
## This keeps attributes of the matrix intact. 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function (inv) inverse <<- inv
  getinv <- function () inverse
  list(set= set, get=get,
       setinv= setinv,
       getinv=getinv)
}

## Calculate matrix inverse, if stored skip calculation


cacheSolve <- function(x, ...) {
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("obteniendo datos cache")
    return(inverse)
  }
  data <-x$get()
  inverse <- solve(data, ...)
  x$setinv(inverse)
  inverse
}