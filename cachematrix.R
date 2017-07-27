## The functions makeCacheMatrix and cacheSolve are used to cache the inverse of a Matrix


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invr <- x$getinverse()
  if(!is.null(invr)) {
    message("getting cached data.")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data)
  x$setinverse(invr)
  invr
}
