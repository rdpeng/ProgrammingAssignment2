## Assignment 2 David A York May 2015
#
## Functions to calculate the inverse of an (invertable) matrix (if it hasn't 
## been done previously) and returning the answer, while caching the answer 
## for later use.
#
#  Call: First, makeCacheMatrix(x) where x is the matrix to be inverted the,
#        Second cacheSolve(w) where w is the output of makeCacheMatrix()
#        --or-- call as, cacheSolve(makeCacheMatrix(x))

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## Cache current matrix (if new)
  matinverse <- NULL
  y <- NULL
  set <- function(y) {
    x <<- y
    matinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matinverse <<- inverse
  getinverse <- function() matinverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
##   makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
##   then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {        ## check for a cache value
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)

  x$setinverse(inverse)
  inverse
}
}
