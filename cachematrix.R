## We are going to write a pair of functions that can cache the inverse of a matrix so the less computation should be done.
## The two functions are 'makeCacheMatrix' and 'cacheSolve'

## The first function 'makeCacheMatrix' creates a special "matrix" which is really a list containing functions that help cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  ##set() is a function for inputing or changing to a new matrix 
  set <- function(y) {
    x <<- y
    ##Everytime we empty object 'inver' when the matrix is changed 
    inver <<- NULL
  }
  ##get() is a function for calling the existing matrix
  get <- function() x
  ##setinverse() is a function for storing the inverse of the matrix x
  setinverse <- function(inver_val) inver <<- inver_val
  ##getinverse() is a function for calling the stored inverse of the matrix x
  getinverse <- function() inver
  list(set=set, get=get,  setinverse=setinverse,  getinverse=getinverse)
}


## This second function 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix above and set the matrix to cache via setinverse. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve would retrieve the inverse from the cache by getinverse and skip the computation.

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  ## check whether there is cache
  if(!is.null(inver)) {
    message("The inverse has been calculated, getting the cache")
    return(inver)
  }
  ##If the inverse has been calculated, it will be done below
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
