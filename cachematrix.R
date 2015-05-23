## Put comments here that give an overall description of what your
## functions do

## These functions computes and cache the inverse of a matrix.

## Write a short comment describing this function

## makeCacheMatrix is a function that takes a matrix as input and stores a list of 4 functions.
## set(), get(), setInverse(), getInverse()

## So that the environment in which these functions are defined is the body of the main funcition (makeCacheMatrix).

## set is a function that changes the matrix stored in the main function. 
## "x <<- y" substitutes the matrix x with y (the input) in the main function. 
## If it was "x <- y" it would have substitute the matrix x with y only in the sub-function set. 
## "I <<- NULL" restores to null the value of the Inverse I, because the old Inverse of the old matrix 
## is not needed anymore. The new Inverse needs to be recalculated through the function cacheSolve.

## get is a function that returns the matrix x stored in the main function.

## setInverse and getInverse simply store the value of the input in a variable I into the
## makeCacheMatrix (setInverse) and return it (getInverse).

## To store the 4 functions in the function makeCacheMatrix, we need the function list(), 
## so that when we assign makeCacheMatrix to an object, the object has all the 4 functions.

makeCacheMatrix <- function(x = matrix()) {

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


## Write a short comment describing this function


## Input of cacheSolve is the object where makeCacheMatrix is stored.

## The first thing cacheSolve does is to check whether the Inverse I exists, stored previously with getInverse, 
## and is not NULL. If it exists in memory, it simply returns a message and the Inverse, 

## If it doesnt exists in memory data gets the matrix x stored with makeCacheMatrix.
## Solve computes the Inverse and assigns it to I. 
## x$setInverse(I) stores it in the object generated with makeCacheMatrix.
## And finally the function returns the Inverse I

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
