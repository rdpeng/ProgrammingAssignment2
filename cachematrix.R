#FileName: cachematrix.R
#Author: ACReilly
#Created: Oct 25, 2015
#Modified: Oct 25, 2015

#This file caches the inverse of a matrix. This program can be ...
#   called by another function that would otherwise repeated calculate...
#   the inverse of a matrix

#These functions assumes that the matrix provided is invertible

#makeCacheMatrix cache the inverse of the matrix x. It returns a list...
#   containing functions to:
#   (1) set the value of the vector 
#   (2) get the value of the vector 
#   (3) set the value of the inverse
#   (4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  #This function creates a special "matrix" object ...
  #   that can cache the inverse of x
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) I <<- Inverse
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve either returns the inverse of a matrix. If the inverse...
#   of the matrix is in cache, it gets the inverse from cache and returns... 
#   it. Otherwsise, it calculates the inverse of the matrix and then...
#   stores the inverse in cache

cacheSolve <- function(x, ...) {
        #This function computes the inverse of the special "matrix" ...
        #   returned by makeCacheMatrix; it returns a matrix that...
        #   is the inverse of 'x'
        I <- x$getInverse()
        if(!is.null(I)) {
          message("getting cached data")
          return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}
