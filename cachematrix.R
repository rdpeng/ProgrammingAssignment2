## The first function, MakeCacheMatrix creates a matrix and returns the list of functions
## The 4 functions inside this functions are
## 1.set function to set the matrix
## 2.get function to get the matrix
## 3.setInverse to set inverse of matrix
## 4.getInverse to get inverse of matrix

## This function creates a special "matrix" object that can cache its inverses

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
        x <<- y
       inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,get = get, setInverse = setInverse,getInverse = getInverse)
}


## This function gets input matrix from makeCacheMatrix and returns the inverse of that matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    return(inv)
  }
