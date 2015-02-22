## Author: João Sebastião de Carvalho
## Date: 22/02/2015
## Course: R programming - rprog-011
## Week 3 Assignment 2 - Caching the Inverse of a Matrix


## The function makeCacheMatrix creates and initializes a cached matrix
## inverse ("cached_matrix_inv") in the environment, and binds the functions
## get and set to the input "x" matrix and to that cached inverse
## matrix.

makeCacheMatrix <- function(x = matrix()) {
   cached_matrix_inv <- NULL
   set <- function(y) {
      x <<- y
      cached_matrix_inv <<- NULL
   }
   get <- function() x
   setinv <- function(inv_m) cached_matrix_inv <<- inv_m
   getinv <- function() cached_matrix_inv
   list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function cacheSolve checks if it already exists a cached calculated inverse
## of the input parameter matrix x. If it exists, it will return it.
## Else, it will then calculate the inverse of the input parameter matrix x,
## using the function solve.

cacheSolve <- function(x) {
   ## Return a matrix that is the inverse of 'x'
   m1 <- x$getinv()
   if(!is.null(m1)) {
      message("getting cached data")
      return(m1)
   }
   data <- x$get()
   m1 <- solve(data)
   x$setinv(m1)
   m1
}
