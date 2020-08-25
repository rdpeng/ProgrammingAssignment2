## We are creating special functions which will store the inverse of any given
## matrix and store it in cache.

## To create a special matrix and store its inverse in cache

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## To calculate the inverse or retrieve it from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    j <- x$getInverse()
    if(!is.null(j)){
      message("getting cached data")
      return(j)
  }
    mat <- x$get()
    j <- solve(mat,...)
    x$setInverse(j)
    j
}