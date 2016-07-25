## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
<<<<<<< HEAD
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inve <<- inverse
  getinverse <- function() inve
  list(set = set,get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
## This function calculaate the inverse of the matrix  created by makeCacheMatrix,
##if the inverse already existe of the same matrix then the function return inverse from the cache.
=======

}
>>>>>>> parent of 442d896... cache matrix


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
