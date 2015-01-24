## Put comments here that give an overall description of what your
## functions do

## This function create a special vector that caches the calculation of its inverse

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function()m
  list(set=set,get=get,
      setInverse=setInverse,
      getInverse=getInverse

}


## This function will calculate the inverse of the received square matrix, caching the result in the cachematrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setInverse(m)
  m
}
