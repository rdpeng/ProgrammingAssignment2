## Put comments here that give an overall description of what your
## functions do

## creates a special matrix, which is a list containing a function to
## set the value of matrix
## get the value of matrix
## setinverse function set the nverse value of matrix
## getinverse function get the inverse value of matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  
  set <- function(y) {  
     x <<- y  
     inv <<- NULL  
  }  
  get <- function() x 
  setinverse <- function(invers) inverse <<- invers
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Following function returns the inverse of matrix. It first checks if inverse is already 
## calculated, if yes then it gets the value from cache else it computes the result and sets
## the value in cache using setinverse function.
cacheSolve <- function(x, ...) {  
   ## Return a matrix that is the inverse of 'x'  
  
  inverse <- x$getinverse()  
  if(!is.null(inverse)) {  
   message("getting cached data")  
   return(inverse)  
 }  
 data <- x$get()  
# print(data)
 inverse <- solve(data)  
 x$setinverse(inverse)  
 inverse  
}  


