##The first function, makeCacheMatrix creates a 'matrix', which performs the following:
   ##set the value of the matrix
   ##get the value of the matrix
   ##set the value of the inverse matrix
   ##get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    set.inverse <- function(solve) m <<- solve
    get.inverse <- function() m
    t=list(set = set, get = get,
           set.inverse = set.inverse,
           get.inverse = get.inverse)
  }
  


## The following function calculates the inverse of a matrix,created with the above function.However, it first checks to see if the inverse of the matrix has already been calculated. If so, it gets the inverse of the matrix from the cache and skips the computation. Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse in the cache via the set.inverse function.

cacheSolve <- function(x, ...) {
  
    m <- x$get.inverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$set.inverse(m)
    m
  }

## /END OF LINE //
  