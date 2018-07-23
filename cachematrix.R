## After defining a square matrix, these functions will create an inverse matrix
## and cache it into the global environment as xx. Following this, the second function will
## search for xx through lexical scoping. If it finds xx in the environment, it will return
## that value. If not, it will calculate and display the matrix inversion.

## this function creates a matrix containing a function to set and get the value of the matrix
## and set and get the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) i <<- inverse
      getinv <- function() i
      list(set=set, get=get, setinv=setinv, getinv=getinv)
}

xx <- makeCacheMatrix(x)

## the following function calculates the inverse of the matrix created with the function above.
## Before calculating, the functionc checks if the inverse value is already cached in the
## environment. If so, it skips the computation. If not, it calculates the inverse.

cacheSolve <- function(x, ...) {
      i <- x$getinv()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinv(i)
      return(i)
}       
      ## Return a matrix that is the inverse of 'x'

