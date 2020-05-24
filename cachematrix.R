## These pair of functions caches the inverse of a matrix

## this first 'makeCachematrix' function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  g <- NULL
  set <- function(m){
    x<<-m
    g<<-NULL
  }
  get <- function()x
  setinverse <- function(inverse)g<<-inverse
  getinverse <- function()g
  list(set=set,get=get,setInverse=setinverse,getinverse = getinverse)
}


## Below's 'cachesolve' function computes the inverse of the special matrix returned by the above's 'makeCachMatrix' 
## function if the inverse has already been calculated(and the matrix has not changed), then cachesolve should 
##retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  g <- x$getinverse()
  if(!is.null(g)){
    message("getting cached data")
    return(g)
  }
  mat <- x$get()
  g <- solve(mat,...)
  x$setinverse(g)
  g
}

