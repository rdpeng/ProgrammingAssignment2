## we use two functions: makeCacheMatrix and cacheSolve to cache the inverse of a matrix

## This is used to create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL          #initializing inverse as NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}    #function to get the matrix x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() {inv}    #function to get the inverse of the matrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This is used to get the cache data

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv) # returns inverse value
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
