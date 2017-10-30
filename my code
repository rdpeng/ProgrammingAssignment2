rm(list = ls())

makeCacheMatrix <- function(ma = matrix()) {
  im <- NULL
  setMatrix <- function(y) {
    ma <<- y
    im <<- NULL
  }
  getMatrix <- function() ma
  setinverse <- function(inv) im <<- inv
  getinverse <- function() im
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if (!is.null(im)) {
    message("getting cached inverse matrix")
    return(im)
  }
  data <- x$getMatrix()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##testing the functions
B <- matrix(c(1,2,3,4),2,2)

B1 <- makeCacheMatrix(B)
cacheSolve(B1) #inverse returned after computation, no message 

cacheSolve(B1) #inverse returned from cache and message is printed here

B2 <- makeCacheMatrix(-B)
cacheSolve(B1)
cacheSolve(B2)
