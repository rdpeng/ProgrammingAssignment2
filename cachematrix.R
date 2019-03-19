## The two main functions, "makeCacheMatrix" and "cacheSolve" enable an computationally efficient way to retrieve the inverse of a square matrix, by storing it in a cache. 
# This is done by implementing a function, enabling the caching of the inverse of a matrix "makeCacheMatrix" and the function "cacheSolve",
# which computes the inverse of that matrix, if required.

## This function creates a matrix with the possibility to cache the inverse of the matrix. 
# Note that the input matrix must be a square and invertible matrix!

makeCacheMatrix <- function(x = matrix()) {
      inv_m = NULL
      set <- function(y) {
            x <<- y
            inv_m <<- NULL
      }
      get <- function() x
      setinv <- function(solved) inv_m <<- solved
      getinv <- function() inv_m
      list(set = set, get = get, 
           setinv = setinv, 
           getinv = getinv)
}

## This function gets the above created matrix as an input and outputs the inverse of that matrix, in the case that it has not been already created.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv_m <- x$getinv()
      if(!is.null(inv_m)) {
          message("getting cached data")
          return(inv_m)
      }
      data <- x$get()
      inv_m <- solve(data,...)
      x$setinv(inv_m)
      inv_m
}
