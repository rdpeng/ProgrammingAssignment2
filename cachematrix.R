## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverted <- NULL
  set <- function(y) {
    x <<-y
    inverted <<-NULL
  }
  get <- function() x
  set_inverted <- function(invert) inverted <<-invert
  get_inverted <- function() inverted
  list(set = set, get = get, setinverted = set_inverted, getinverted = get_inverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverted <- x$getinverted()
  if(!is.null(inverted)) {
    print("Using cached data")
    return(inverted)
  }
  the_matrix <- x$get()
  inverted <- solve(the_matrix)
  x$setinverted(inverted)
  inverted
}
