## Put comments here that give an overall description of what your
## functions do

## Here we have two functions that are used to create a special object that stores a matrix 
## and cache's its inverse.
## The first function, "makeCacheMatrix" creates a special "matrix", which is actually a list 
## containing a function to: 1) Set the value of a matrix; 2) Get the value of a matrix; 3) Set
## the inverse of a matrix, and 4) Get the inverse of a matrix. 


makeCacheMatrix <- function(x = matrix()) {
inv.matrix <- NULL
  set <- function(y) {
    x <<- y
    inv.matrix <<- NULL
  }
  get <- function() x
  set.inv <- function(solve) inv.matrix <<- solve
  get.inv <- function() inv.matrix
  list(set = set, get = get,
           set.inv = set.inv,
           get.inv = get.inv)

}


## "cacheSolve" calculates the inverse of the special "matrix" created by the above function 
## and caches its value. But first it checks if the inverse of the matrix has already been 
## calculated, and if it is so, it returns the cached value and skips the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv.matrix <- x$get.inv()
  if(!is.null(inv.matrix)) {
    message("getting cached data")
    return(inv.matrix)
  }
  data <- x$get()
  inv.matrix <- solve(data, ...)
  x$set.inv(inv.matrix)
  inv.matrix
}
