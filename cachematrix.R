## This file contains two R functions
##      makeCacheMatric takes in a matrix to create a special "matrix" that is a list containing several functions
##      cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix using those functions

## Usage: mcm <- makeCacheMatrix(matrix(c(1:4), nrow=2))
##        cacheSolve(mcm)

## makeCacheMatric takes in a matrix to create a special "matrix" that is a list containing a function to
##     set the value of the matrix
##     get the value of the matrix
##     set the value of the inverse matrix
##     get the value of the inverse matrix

makeCacheMatrix<- function(x = matrix()) {
     s <- NULL
     ## set the value of the matrix
     set <- function(y) {
          x <<- y
          s <<- NULL
     }
     get <- function() x          ## get the value of the matrix
     setSolve <- function(solve) s <<- solve          ## set the inverse matrix
     getSolve <- function() s          ## get the inverse matrix
     ## return the list
     list(set = set, get = get,
          setSolve = setSolve,
          getSolve = getSolve)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above
##      If the inverse has already been calculated (and the matrix has not changed)
##      then the cachesolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
     ## determine if the inverse of the matrix is in cache by calling the function from makeCachematrix
     s <- x$getSolve()
     ## if there is a value in cache, return it and do no other work
     if(!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     ## else the inverse was not in cache and it must be caluculated
     data <- x$get()                ## get the matrix
     s <- solve(data, ...)          ## get the inverse of the matrix
     x$setSolve(s)                  ## store the inverse matrix in cache
     s                              ## return the inverse matrix
}
