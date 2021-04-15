## Assignment for R programming Week 3

## This function caches the inverse of the special matrix created 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
}
get <- function()x
setInverse <- function(inverse) m <<- inverse
getInverse <- function() m 
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The inverse of the special matrix returned by the function above
## is computed by the one below
## If the inverse is calculated, cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}
