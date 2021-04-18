## Assignment for R programming Week 3

## This function caches the inverse of the special matrix created 

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y){
    x <<- y
    k <<- NULL
}
get <- function()x
setInverse <- function(inverse) k <<- inverse
getInverse <- function() k 
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The inverse of the special matrix returned by the function above
## is computed by the one below
## If the inverse is calculated, cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  k <- x$getInverse()
  if(!is.null(k)){
    message("getting cached data")
    return(k)
  }
  mat <- x$get()
  k <- solve(mat,...)
  x$setInverse(k)
  k
}
