## these functions create an inverse of a given matrix. They cache the inverse
## so that the calculations won't be repeated each time the function is run.

## This function creates a list of four functions that sets and gets the data
## then sets and gets the inverse its inverse

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
  x <<- y
  i <<- NULL
}
get <- function() x
setinverse <- function(inverse) i <<- inverse
getinverse <- function() i
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


## This function gives the inverse of the defined matrix. However, it firstly
## checks the memory for the inverse and if it is already calculated, it just
## gets it from the memory and prints "getting cached data"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
