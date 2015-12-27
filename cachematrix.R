## These two functions together automate the process of creating an object that can cache the inverse
## of a matrix in the first function, and then calculating the inverse - and caching it - in the second
## if the second function finds a previously calculated inverse for that same matrix, it will skip
## calculations and pull the inverse from the cache.

## This function establishes the cache for the matrix and its potential inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function gets the inverse of the matrix. 
## If the inverse has already been calculated, it skips the calculation
## and pulls the previously calculated inverse of the matrix. 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
