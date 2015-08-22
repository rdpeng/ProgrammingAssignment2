## This function creates a special matrix object, 
## named "CacheMatrix", that stores (chaches) the inverse of itself.
# It includes 4 sub-functions:
# getting and setting the matrix itself
# getting and setting the inverse matrix

## Written by Uri Lerner as part of the Coursera R programming course (031)

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function(inverse) inv <<- inverse
      getinv <- function() inv
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}

## This function calculates the inverse matrix of a given matrix.
# The matrix is stored as a 'CacheMatrix' object
# If inverse was previously calculated for the given matrix
# Stored (Cached) value will be returned

## Written by Uri Lerner as part of the Coursera R programming course (031)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv
}
