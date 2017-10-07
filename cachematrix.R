## These functions create a matrix and its inverse.
## The inverse is cached, so the computations can be faster.


## This function creates a "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  inverso <- NULL
  set <- function(y){
    x <<- y
    inverso <<- NULL
  }
  get <- function() x
  setInverse <- function(invMatriz) inverso <<- invMatriz
  getInverse <- function() inverso
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)


}


## This function calculates the inverse of the matrix returned by 
## the function makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  inverso <- x$getInverse()
  if(!is.null(inverso)){
    return(inverso)
  }
  matrizInv <- x$get()
  inverso <- solve(matrizInv)
  x$setInverse(inverso)
  inverso    
  
  
}
