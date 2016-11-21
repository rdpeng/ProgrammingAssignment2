## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {                            ## set the value of the vector
    
    x <<- y
    
    m <<- NULL
    
  }
  
  get <- function() x                             ## get the value of the vector
  
  setinverse <- function(inverse) m <<- inverse   ## set the value of inverse
  
  getinverse <- function() m                      ## get the value of inverse
  
  list(
    
    set = set,
    
    get = get,
    
    setinverse = setinverse,
    
    getinverse = getinverse)
  
}



## Write a short comment describing this function

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  if(!is.null(m)) {                        ## see if the inverse has already been calculated.
    
    message("getting cached data")         ## if the inverse has already been calculated,message
    
    return(m)                              ## and return the inverse from the cache.
    
  }
  
  data <- x$get()         ## if the inverse not yet calculated, 
  
  m <- solve(data, ...)   ## solve function returns the inverse.
  
  x$setinverse(m)         ## sets the value of the inverse in the cache via the setinverse function
  
  m
}
