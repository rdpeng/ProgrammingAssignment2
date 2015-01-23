

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

z<-matrix(c(1,2,3, 2,3,2, 3,2,1), nrow = 3, ncol = 3)
h<-as.data.frame(z)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setM <- function(y) {
    x <<- y
    m <<- NULL
  }
  getM <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(setM = setM, 
       getM = getM,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<- x$getinv
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}



