## Programming in R Coursera Course, Programming Assignment 2
## Edited by: Antal Nusselder
## Last edit date: 21-8-2015
## Containing two functions that cache the inverse of a matrix in order to save on computational power

## This first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

      m <- NULL
      set <- function(y) {					## set the value of the matrix
              x <<- y
              m <<- NULL
      }								
      get <- function() x					## get the value of the matrix
      setmatrix <- function(solve) m <<- solve		## set the value of the inverted matrix
      getmatrix <- function() m				## get the value of the inverted matrix
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)

}


## This second function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {

      m <- x$getmatrix()
      if(!is.null(m)) {						## check if this matrix has already been inverted
              message("getting cached data")		## if so, retrieve it's inverted value
              return(m)
      }								
      matrix <- x$get()
      m <- solve(matrix, ...)					## calculate the inverted matrix
      x$setmatrix(m)						## set the value of the inverted matrix in the cache
      m

}
