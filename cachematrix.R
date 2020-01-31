## The functions here are to create a matrix object and cache its inverse. It till store the 
## inverse until the matrix has been changed

## Write a short comment describing this function
# makeCacheMatrix: This function containts 4 parts while using a matrix as an input, the first part 
# set the value of the matrix and the second will get that value. The third and fourth do similar 
# but with the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  setMat <- function(y) {                                             ##sets the value of the matrix
    x <<- y
    invM <<- NULL
  }
  getMat <- function() x                                              ##gets the value of the matrix
  setinv<- function(inverse) invM <<- inverse                 ##sets the value of the ivnerse matrix
  getinv <- function() invM                                   ##gets the value of the inverse matrix
  list(setMat = setMat, getMat = getMat,
       setinv = setinv,
       getinv = getinv)
}

## Write a short comment describing this function
#cacheSolve: This functions allows us to compute the inverse of the matrix returned above.If this 
#inverse has already calculated it will return the inverse from the cache.

cacheSolve <- function(x, ...) {
  invM <- x$getinv()
  if(!is.null(invM)) {                    ##checks to see if the inverse has already been calculated
    message("getting cached data") 
    return(invM)                   ##returns the calculated inverse from cache if already calculated
  }
  data <- x$getMat()
  invM <- solve(data, ...)                                    ##calculates the inverse of the matrix
  x$setinv(invM) 
  return(invM)                                         ## Return a matrix that is the inverse of 'x'
}
