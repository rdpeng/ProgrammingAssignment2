
##This part of the code caches the inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(cMatrix) m <<- cMatrix
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This part of the code solves the inverse of the matrix if there is no cache version, otherwise it returns the cached matrix.
=======
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
>>>>>>> 1eee67d11ccff0184ad7ef470128795d39811b48

=======
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

>>>>>>> 1eee67d11ccff0184ad7ef470128795d39811b48
makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
