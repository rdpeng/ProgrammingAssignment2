## These two functions check to see if the inverse of a square invertible matrix
## has been cached.

## makeCacheMatrix takes a matrix and returns a list of four functions that 
## can be used to store the matrix object that you wish to find the inverse of, 
## return this matrix object, store the inverse of the matrix object, 
## and return the inverse.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y){
            x <<- y
            i <<- NULL }
      get <- function(){x}
      setinv <- function(inv) i <<- inv 
      getinv <- function(){i}
      list(set = set, get = get, setinv = setinv, getinv = getinv)
      }



## cacheSolve takes a list of functions as an argument and returns either the
## the cached inverse of the matrix object or solves for the inverse of the matrix
## and then caches the inverse matrix.

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)){
            message("getting cached data")
            return(i)
            }
      matrix <- x$get()
      inv <- solve(matrix)
      x$setinv(inv)
      inv 
      }

