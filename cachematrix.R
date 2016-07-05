# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it 
# repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions 
# that cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invers <- NULL
  set <- function(y) {
    x <<- y
    invers <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invers <<- inverse
  getinverse <- function() invers
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the 
# inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
  invers <- x$getinverse()
  if(!is.null(invers)) {
    message("getting cached data.")
    return(invers)
  }
  data <- x$get()
  invers <- solve(data)
  x$setinverse(invers)
  invers
        ## Return a matrix that is the inverse of 'x'
}
