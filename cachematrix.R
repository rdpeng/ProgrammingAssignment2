## These two functions calculate inverse of matrices.The functions do so 
## by caching the inverses of the matrices.

##This function calculates the inverse of square matrices

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL()
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function finds the inverse of the matrix returned by the first function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if (!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
