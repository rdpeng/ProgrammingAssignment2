## These functions work together to make a special matrix with
## a cached copy of it's inverse.
## This can save time recalculating the inverse repeatedly.

## This function creates a variable to hold a matrix and
## the inverse of that matrix.
makeCacheMatrix <- function(matrix_x = matrix()) {
  inverse_x <- NULL
  set <- function(y) {
    matrix_x <<- y  #scoping
    inverse_x <<- NULL #scoping
  }
  get <- function() return(matrix_x)
  setinverse <- function(inverse_x) matrix_x <<- inverse_x  #scoping
  getinverse <- function() return(inverse_x)
  list(set = set, get = get,
       setmean = setinverse,
       getmean = getinverse)
}

## This function returns a matrix that is the inverse of 
## the given matrix.
## The first time it uses CPU power to calculate then cache the inverse.
## After the first time it is called, the cached inverse is returned.
cacheSolve <- function(matrix_x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- matrix_x$getinverse()
        if(!is.null(inverse_x)) {
            message("Getting cached data...")
           return(inverse_x)
        }
        x <- matrix_x$get()
        inverse_x <- solve(x, ...)
        matrix_x$setinv(inverse_x)
        return(inverse_x)
}
