## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
## A list containing four functions to set and get the value of the matrix
## and to set and get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
N <- NULL
## Define function to set the value of the matrix. It also clears the old
## inverse from the cache   
 set <- function(y) {
      x <<- y
      N <<- NULL
    }
## Define function to get the value of the matrix
    get <- function() x
## Define function to set the inverse. This is only used by getinverse()
## when there is no cached inverse
    setInverse <- function(inverse) N <<- inverse
## Define function to get the inverse
    getInverse <- function() N
## Return a list with the above four functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Return inverse of matrix x

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
N <- x$getInverse() ## fetches the cached value for the inverse
  if (!is.null(N)) { ## if the cache was not empty, we can just return it
    message("getting cached data")
    return(N)
  }
## Cache was empty. We need to calculate it, cache it, & return it.
  mat <- x$get() ## get value of matrix
  N <- solve(mat, ...) ## calculate the inverse
  x$setInverse(N) ## cache the result
  N
}
