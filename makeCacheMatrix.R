
#!/usr/bin/R


# R pgming: assignment 2

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # store the cached inversed matrix in inv
  inv <- NULL

  # set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  # get the matrix
  get <- function() x

  # set for the invese
  setinverse <- function(inverse) inv <<- inverse

  # get the inverse
  getinverse <- function() inv

  # return the matrix with newly defined function
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()

  # check if the inverse is already computed.
  # if (yes)
  # 	get the cached data
  # else
  # 	compute and get
  if(!is.null(inv)) {
    message(" matrix invesrion computed, get cached data.")
    return(inv)
  }

  # compute the invese and return it
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
