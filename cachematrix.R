## Calculates inversed matrix, store value in cache value

## In fact, makeCacheMatrix encapsulates input matrix and it's inversed matrix
## Inversed matrix cached. If input matrix reseted, cache is cleared.

makeCacheMatrix <- function(matr = matrix()) {
  inversed <- NULL              #Inversed matrix, cached
  set <- function(y) {          #New input matrix, clear cache 
    matr <<- y
    inversed <<- NULL
  }
  
  get <- function() matr
  setinverse <- function(inverse) inversed <<- inverse
  getinverse <- function() inversed
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Try to get cached inversed matrix; if fails - recalculates it and caches it back.
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
