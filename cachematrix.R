## Makecachematrix creates the cachematrix. The actual calculation of inverse happens in cachesolve function.

## Initialize objects
## set() takes an argument that is named as y. set() we use the <<- , which assigns the value on the right side of the operator to an object in the parent environment named by the object on the left side of the operator.
## setinverse and getinverse define the setter and getter for the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cachesolve() is required to populate or retrieve the inverse from makecachematrix()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  library("MASS", lib.loc="C:/Program Files/R/R-3.5.3/library")
  data <- x$get()
  i <- ginv(data, ...)
  x$setinverse(i)
  i
}
