## Put comments here that give an overall description of what your
## functions do

## Function create a special matrix object, 
## containing a list of function:
##   1. to set the value of the matrix
##   2. to get the value of matrix
##   3. to chache the value of the inverse of the function
##   4. to retrieve the value of the chached inverse

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     get <- function() x 
     set <- function(y) {
          x <<- y
          inv <<- NULL 
     }
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     
     
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## The function casheSolve returns the chached inversed of x if it exits,
## otherwise computes the inverse of x, using "solve" function and return.


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     inv <- x$getInverse()
     
     if (!is.null(inv)) {
          ## The cached inverse exists. Retrive it.
          message("Getting chached data")
          return(inv)
     } 
     ## The cached inverse does not exist. Compute it.
     
     
     inv <- solve(x$get())
     x$setInverse(inv)
     inv
}
