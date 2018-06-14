## In this file, there are two functions: makeCacheMatrix and cacheSolve.
## makeCacheMatrix function: Matrix inversion is computationally intensive. 
## To avoid recomputing the inverse for getting the same result repeatedly, 
## we can simply compute the result once, store/Cache the result and reuse 
## this pre-computed result.
## cacheSolve function: After creating the matrix, use the cacheSolve function 
## to compute the inverse and cache the result. If you try using cacheSolve 
## again on the same matrix, then the pre-computed result is obtained
## instead of recomputing it.


## Following the same format as the assignment example,"makeCacheMatrix" 
## function is created that consists of following four steps:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
## 5. followed by capturing all of them in a "list".

makeCacheMatrix <- function(x = matrix()) {
	  
  ## Initially set to NULL that changes when the user sets the value
  inv <- NULL
  
  ## 1. Set the matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## 2. Get the matrix 
  get <- function() x
  
  ## 3. Set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  ## 4. Get the inverse
  getinverse <- function() inv
  
  ## 5. Capture them in a list
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)  
}


## To return a matrix that is the inverse of 'x', following 
## the format as in the assignment example. 
## 1.Get the current state of the inverse and see if it has been 
## computed yet or not.
## 2. If the inverse is already computed,return the pre-computed 
## inverse and prompt a message "getting chached matrix".
## 3.If inverse is not yet computed, compute it by geting the 
## matrix and finding its inverse, and cache the results.Return 
## the chached result.

cacheSolve <- function(x, ...) {
        data <- x$get()
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
 
  if(!is.null(inv)) {		
    message("Getting cached matrix")
    return(inv)
  }
  
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv    ## Return a matrix that is the inverse of 'x'
        
}
