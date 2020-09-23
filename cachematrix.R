## Put comments here that give an overall description of what your
## functions do
##I call the function with a matrix, compute the inverse, retrieve the inverse from the cache list, 
##change the call matrix to the inverse, compute the inverse on that and return the original function.

## Write a short comment describing this function
##Set and get the value of the matrix, set the value and get the value of the inverse

rm(list = ls())

makeCacheMatrix <- function(x = matrix()) {
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


## Write a short comment describing this function
##This function computes the inverse of the special “matrix” returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

cacheSolve(B1) #inverse returned from cache and message 

B2 <- makeCacheMatrix(-B)
cacheSolve(B1)
cacheSolve(B2)