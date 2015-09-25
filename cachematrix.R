## This assignment is about caching the Inverse of a Matrix
## The objective is that any costly or memory intensive operations can be 
## cached for repeated use so that they need not be executed again and again.
## The implementation of the functions given here can be used to store 
## the inverse of a matrix cache as the inverse operation is usually intensive.

## makeCacheMatrix function. This sets and gets the inverse value of the input matrix 
## to and from the cache.


makeCacheMatrix <- function(x = matrix()) {
  
## Initialize
  inv <- NULL
  
## set the input matrix 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
## return the set matrix  
  get <- function() x
  
## set the inverse object
  setInverse <- function(inverse) inv <<- inverse 
  
## return the inverse object
  getInverse <- function() inv 
  
## The vector that contains the above values  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

## This function computes the inverse of the input matrix.
## If the inverse has already been calculated and the matrix has not been changed,
## then it should retrieve the inverse from the cache.
## If it is the first time that this function is called, then it will compute
## the inverse using "solve" and sets the computed value in cache
## By setting the value in cache when getInverse of previous function is called 
## it will provide the cached value.

cacheSolve <- function(x, ...) {
       
## First assign the getInverse return value to inv
  
    inv <- x$getInverse()

## Verify whether it is not null. 
## If so then it is already available in cache    
        
  if (!is.null(inv)) {
    message("Obtain the cached data")
    return(inv)
  }
    
## Get the input matrix
  mat <- x$get()
  
## Obtain inverse by calling solve
  inv <- solve(mat, ...)
  
## Set the inverse in cache
  x$setInverse(inv)
  
## Return a matrix that is the inverse of 'x'
  inv
  
}
