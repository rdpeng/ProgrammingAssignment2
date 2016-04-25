## Put comments here that give an overall description of what your  
## functions do  
## functions do:  Return the inverse of matrix with determinant that is not zero, and save the result in cache  
## Write a short comment describing this function  
    
 makeCacheMatrix <- function(x = matrix())   
## This function creates a special "matrix" object that can cache its inverse.  
          
makeCacheMatrix <- function(m = matrix()) {  
  inverse <- NULL  
  set <- function(y) {  
 m <<- y  
 inverse <<- NULL  
 }  
get <- function() m  
setInverse <- function(solve) inverse <<- solve  
getInverse <- function() inverse  
list(set = set, get = get,  
setInverse = setInverse,  
getInverse = getInverse)  
}  
    
## Write a short comment describing this function  

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix,  
## If the inverse has already been calculated (and the matrix has not changed),  
## then the cachesolve should retrieve the inverse from the cache.  
cacheSolve <- function(x, ...) 
## Return a matrix that is the inverse of 'x'  
cacheSolve <- function(m, ...) {  
inverse <- m$getInverse()  
if(!is.null(inverse)) {  
message("getting cached data")  
return(inverse)  
  }  
data <- m$get()  
inverse <- solve(data, ...)  
m$setInverse(inverse)  
inverse  
  }  










