## Assignment #2 has two functions calculates and caches the inverse of a matrix.


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(mi) i <<- mi
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mi <- x$getInverse()
  if(!is.null(mi)) {
    message("getting cached data")
    return(mi)
  }
  data <- x$get()
  mi <- solve(data,...)
  x$setInverse(mi)
  mi
}
