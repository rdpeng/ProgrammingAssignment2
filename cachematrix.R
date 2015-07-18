## JHU Coursera R Programming Assignment #2
## July 2015
## author: leonard.m@greski.com
##
## Invert a matrix and cache its result, so subsequent requests for matrix inversion
## retrieve from cache rather than redoing the inversion calculation

## makeCacheMatrix() - create a cache for a matrix 

makeCacheMatrix <- function(x = matrix()) {
     theMatrix <- NULL
     set <- function(y) {
          x <<- y
          theMatrix <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) theMatrix <<- solve
     getsolve <- function() theMatrix 
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)

}


## cacheSolve() invert a matrix, or retrieve from cache if it already exists

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
    
      ## Attempt to retrieve matrix from cache
     theMatrix <- x$getsolve()
     if (!is.null(theMatrix)) {
          message("getting cached inverse")
          return(theMatrix)
     }
     ## if we get past the if() statement, the cache is empty
     
     ## invert the matrix, set the cache, and return
     data <- x$get()
     theMatrix <- solve(data)
     x$setsolve(theMatrix)
     theMatrix
}
