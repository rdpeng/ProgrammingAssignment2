## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ##Creating a special matrix object, a list of functions to set and get the elements of a matrix and its inverse
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inve) inv <<- inve
  get_inverse <- function() inv
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



## Write a short comment describing this function
## this function will return the inverse of the matrix immediately if it's not null
## or it'll compute the inverse, cache it and then return it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inverse()
        if(!is.null(inv)){
          message("Getting Cached Data")
          return (inv)
        }
        
        data <- x$get()
        inve <- solve(data)
        x$set_inverse(inve)
        inve
}
