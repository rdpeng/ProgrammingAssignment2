## the 2 functions defined below are used to store data in the cache and read data from the cache.
## a matrix and its inverse can be stored and read using these functions.




## Short description of "makeCacheMatrix":

## Argument is a matrix. 

## 1st step: Inverse can only be calculated for a square matrix, so I first test if ncol
## is equal to nrow. If not, I only give a warning, since the matrix can still be stored in the cache
## and this might be used by another function.

## 2nd step: defining functions
##            1. "set": store the value of the matrix in the cache
##            2. "get": get the value of the matrix from the cache
##            3. "setinv": store the value of the inverse of the matrix in the cache
##            4. "getinv": get the value of the inverse of the matrix from the cache

## 3rd step: a list is returned containing these four functions. In the environment of the function
## "makeCacheMatrix" the variable "x" has the value of the argument of the function.

makeCacheMatrix <- function(x = matrix()) {
  test <- nrow(x) == ncol(x)
  if(test != TRUE) {message("Warning: square matrix needed for inverse calculation, change of argument recommended!")}
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv_new) inv <<- inv_new
    
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Short description of "cacheSolve":

## Argument is the list created by "makeCacheMatrix"

## 1st step: testing if the inverse is already stored in the cache by calling the function
## x$getinv(). If this returns a value apart from NULL, then the stored inverse matrix is returned
## and nothing else is done

## from here on only if x$getinv() returns NULL:

## 2nd step: the matrix is read by x$get(). 

## 3rd step: also in this function it's tested if it's a square matrix. if it isn't, inverse is set
## to NULL. Otherwise the inverse is calculated using the solve() function.

## 4th step: the result for the inverse is then stored in the cache using x$setinv(inverse)

## 5th step: finally, the function returns the inverse matrix of the matrix which was the argument
## of the function "makeCacheMatrix"

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  test <- nrow(data) == ncol(data)
  
  if(test != TRUE){    
    message("Warning: square matrix needed for inverse calculation, inverse set to NULL")
    inverse <- NULL
    x$setinv(inverse)
    inverse
  } else {
  inverse <- solve(data)
  x$setinv(inverse)
  inverse
  }
}
