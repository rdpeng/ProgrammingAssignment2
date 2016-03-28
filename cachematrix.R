## Put comments here that give an overall description of what your
## functions do

## make cache matriz

makeCacheMatrix <- function(x = matrix()) {

m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) 
  

}


## cachesolve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        x<-matrix(runif(1:16), nrow=4, ncol=4)
#4

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
