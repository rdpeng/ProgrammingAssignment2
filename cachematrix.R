## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the matirx
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y){
    x <<- y
    m <<- NULL
  }
  get <- function()x
  setinverse <- function (inverse) m <<- inverse
  getinverse <- function()m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}

## The following function cacheSolve calculates the inverse of the 
## the matrix created with the above function it first checks to 
## see if the inverse of the matrix has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$inverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setinverse(M)
  m
}

