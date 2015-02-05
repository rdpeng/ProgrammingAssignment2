## Put comments here that give an overall description of what your
## functions do

## In fact, it encapsulates input matrix and it's inversed matrix
## Inversed matrix cached. If input matrix reseted, cache is cleared.

makeCacheMatrix <- function(matr = matrix()) {
  inversed <- NULL              #Inversed matrix, cached
  set <- function(y) {          #New input matrix, clear cache 
    matr <<- y
    inversed <<- NULL
  }
  
  get <- function() matr        #input matrix getter
  setinverse <- function(inverse) inversed <<- inverse   #inversed matrix setter
  getinverse <- function() inversed                      #inversed and cached matrix getter
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  # list of methods
  
}


## Try to get cached inversed matrix; if fails - recalculates it and set it back to cache.

cacheSolve <- function(matr, ...) {
  ## Return a matrix that is the inverse of 'matr'
  inversed <- matr$getinverse()
  
  if(!is.null(inversed)) {    #We have cached result, so return it
    message("getting cached data")
    return(inversed)
  }
  data <- matr$get()          #We don't have cached result, so re-calculate it ...
  inversed <- solve(data)
  matr$setinverse(inversed)   #... and cache for future use
  
  inversed
}
