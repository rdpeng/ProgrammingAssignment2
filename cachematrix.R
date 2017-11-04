# Week 3 Assignment
# Second repo copy
# The objective is to write two functions that work in conjunction to calculate the inverse of matrix
# The first function, called makeCacheMatrix creates the place to cache the inverse of the matrix that is created
# The second function computes the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  # Create a special matrix that stores a numeric vector and caches its mean
  # initialize inmat as NULL
  # define the function that will cache the matrix and then reinitializes invmat
  
  invmat <- NULL
  set <- function(y) {
    x <<- y 
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  # Create a function that will calcualte the inverse of the matrix if it has changed
  
  invmat <= x$getinverse()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data, ...)
  x$setinverse(invmat)
  invmat
}
