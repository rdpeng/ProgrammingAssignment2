## the two functions below are able to ceate a special matrix. If the inverse of the matrix is not computed already, the inverse will be calculated. Both the content of the matrix and its inverse are stored for possible later usage.


##First funciton makeCachMatrix()
#The input is an invertible matrix
## The outut of the function is a list of four functions
##        1 set the value of the matrix
##        2 get the value of the matrix
##        3 set the value of the inverse matrix
##        4 get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
 
  m.inv <- NULL
  set <- function(y) {
    x <<- y
    m.inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m.inv <<- inverse
  getinv <- function() m.inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## 2nd funciton CacheSolve()
## The function calculates the inverse of the special "matrix" created by the above function only if the inverse is not yet computed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m.inv <- x$getinv()
  if(!is.null(m.inv)) {
    message("getting cached data")
    return(m.inv)
  }
  m.data <- x$get()
  m.inv <- solve(m.data, ...)
  x$setinv(m.inv)
  m.inv
}

