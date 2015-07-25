## a pair of functions that cache the inverse of a matrix.

##creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat = matrix()) {
  
  ## set to NULL
  matrixinverse <- NULL
  
  set <- function(y) {
    
    mat <<- y
    
    matrixinverse <<- NULL
    
  }
  
  get <- function() mat
  
  setInverse <- function(solve) matrixinverse <<- solve ## calculate the inverse of matrixinverseatrix
  
  getInverse <- function() matrixinverse
  
  list(set = set, get = get,
       
       setInverse = setInverse,
       
       getInverse = getInverse)
  
}



## Return Matrix that is the inverse of 'mat'

cacheSolve <- function(mat, ...) {
  
  matrixinverse <- mat$getInverse()
  ## test if matrixinverse exist in cache
  if(!is.null(matrixinverse)) {
    
    message("getting cached data")
    
    return(matrixinverse)
    
  }
  
  data <- mat$get()
  
  matrixinverse <- solve(data, ...)
  
  mat$setInverse(matrixinverse)
  
  matrixinverse
  
}


