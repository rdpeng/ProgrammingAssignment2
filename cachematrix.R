# This makeCacheMatrix function attempts to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This following function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. 
# If not, it computes the inverse and sets the value in the cache via
# setinverse function.
# Moreover, this function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Here is a test run:
x = rbind(c(1, 2), c(3, 4))
m = makeCacheMatrix(x)
m$get()

## No cache in the first run
cacheSolve(m)

## Retrieving from the cache in the second run
cacheSolve(m)
