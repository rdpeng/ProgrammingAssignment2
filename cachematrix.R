## For Coursera Data Science: R Programming, 3rd Week 
## Beginning on 16th April 2020 by RAHUL JHA
 

## The function is written for Peer-Graded Assignment 
## For a pair of functions that cache the inverse of a matrix.

## "makeCacheMatrix" Function

## The function "makeCacheMatrix" is used to create a special matrix 
## object that is used to cache the matrices' inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ivr <- NULL
  set <- function(y) {
    x <<- y
    ivr <<- NULL
    
  }
  
  get <- function() x
  setinverse <- function(inverse) ivr <<- inverse
  getinverse <- function() ivr
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
       
}

## "cacheSolve" Function

## This function "cacheSolve" finds the inverse of the special matrix returned by makeCacheMatrix.
## If the inverse has already been calculated of a matrix used previously,
## then the cacheSolve will bring back the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Returns a matrix that is the inverse of 'x'
  
  ivr <- x$getinverse()
  if(!is.null(ivr)) {
    message("getting cached data")
    return(ivr)
  
  }
  
  data <- x$get()
  ivr <- solve(data, ...)
  x$setinverse(ivr)
  ivr
  
}
