## The following function goal is to compute the inverse of a square matrix 
## under the assumption that the matrix supplied is always invertible

## makeCacheMatrix returns a list containing functions for storing
## a matrix and its inverse matrix using getters and setters

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## cacheSolve checks whether an inverse matrix was calculated for
## an input matrix (given as a list of functions with cached values),
## if inverse was calculated, returns the value, if not, 
## calculate and return the inverse matrix

cacheSolve <- function(x, ...) {
      if (!is.null(x$getinverse())){
            message("inverse was already cached")
            return(x$getinverse())
      }
      inverse<-solve(x$get(),...)
      x$setinverse(inverse)
      inverse
        ## Return a matrix that is the inverse of 'x'
}
