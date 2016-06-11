## Caching the Inverse of a Matrix:

## This function creates a special "matrix" object that can cache its inverse.
## The special"matrix" is named as "Speacial_mat"
## Inverse is named as "Inv_mat"

makeCacheMatrix <- function(Special_mat = matrix()) {
  Inv_mat <- NULL
  
  set <- function(new_mat) {
    
    Special_mat <<- new_mat
    Inv_mat <<- NULL
    Special_mat
  }
  
  get <- function() Special_mat
  
  setinverse <- function(inverse){
    Inv_mat <<- inverse
   
  }
  
  getinverse <- function() Inv_mat
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the 'Special_mat' created by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed)
##then it should retrieve the 'Inv_mat' from the cache.

cacheSolve <- function(Special_mat) {
  ## Return a matrix that is the inverse of 'Special_mat'
  
  Inv_mat <- Special_mat$getinverse()
 
  if(!is.null(Inv_mat)) {
    cat("getting cached data")
    return(Inv_mat)
  }
  
  data <- Special_mat$get()
  Inv_mat <- solve(data)
  Special_mat$setinverse(Inv_mat)
  Inv_mat
}

