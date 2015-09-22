## This is a solution to the Programming Assignment 2
## I took the framework straight from the assignment example
## and only changed var names and calls from "mean" to "solve"/"inverse"
## but the solution works

## Instantiates a "special matrix" object with 4 methods
## (data setter, data getter, inverse setter and inverse getter)
## line 15 ensures that every time data is changed, the inverse is reset,
## therefore cachSolve will re-calculate it as opposed to using a cached value

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## Takes "special matrix" as an argument;
## tries to loop up inverse (stored previously) using the getter method, if found, returns it;
## if not found, calculates the inverse using native solve() and stores is
## in the "super-matrix" instance using the setter method

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inverse(inv)
  inv
}