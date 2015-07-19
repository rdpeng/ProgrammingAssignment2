## JHU Coursera R Programming Assignment #2
## July 2015
## author: leonard.m@greski.com
##
## Invert a matrix and cache its result, so subsequent requests for matrix inversion
## retrieve from cache rather than redoing the inversion calculation

## makeCacheMatrix() - create a cache for a matrix 

makeCacheMatrix <- function(x = matrix()) {
     ## initialize theInverse to NULL
     theInverse <- NULL
     ## assign contents of input to cached matrix and NULL the inverse
     set <- function(y) {
          x <<- y
          theInverse <<- NULL
     }
     
     ## setup get, setsolve, and getsolve functions to access cache
     get <- function() x
     setsolve <- function(solve) theInverse <<- solve
     getsolve <- function() theInverse
     
     ## create list with methods for get / set of both original matrix
     ## and its inverse 
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
     
     ## check to see whether matrix is invertible, meaning that
     ## the determinant must be non-zero
     if (det(data) == 0) {
          ## can't invert this matrix, so set the cache to NULL
          ## and return
          message("Determinant is zero, setting cache to NULL")
          x$setsolve(NULL)
          return(NULL)
     } 
     theMatrix <- solve(data)
     x$setsolve(theMatrix)
     theMatrix
}
