## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  # sets the value of i to NULL (provides a default if cacheSolve has not yet been used)
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv) # creates a list to house the four functions
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
         ## Return a matrix that is the inverse of 'x'
}
