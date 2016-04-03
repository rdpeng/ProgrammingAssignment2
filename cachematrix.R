# Purpose: Matrix inversion.

# Details: Matrix inversion is usually a costly computation and there may 
# be some benefit to caching the inverse of a matrix rather than compute it 
# repeatedly. The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# set/get the value of the matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  SetValInverse <- function(inverse) inv <<- inverse
  GetValInverse <- function() inv
  list(set=set, get=get, SetValInverse = SetValInverse, GetValInverse = GetValInverse)
}


# The following function returns the inverse of the matrix. 
# Initially this function checks if the inverse matrix has 
# already been computed and If so, it returns the result 
# without a new computation. If not, it computes the inverse, 
# sets the value in the cache via the SetValInverse function.


# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
  inv <- x$GetValInverse()
  if(!is.null(inv)) {
    message("Retrieve from cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$SetValInverse(inv)
  inv
}