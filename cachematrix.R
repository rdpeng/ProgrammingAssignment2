## Put comments here that give an overall description of what your
## functions do

## a list that s a set of function to get , set, get inverse, set inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <- y
    m <- NULL
  }
  get <- function() x
  getinv <- function() m
  setinv <- function(inv) m <- inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## cache solve function to estimate 
## inverse of a matrix if it is not in the cache else skip the computation 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}
