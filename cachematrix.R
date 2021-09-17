## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions to set and get the 
## value of a matrix and to set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function () x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list (set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function takes the object created with the previous function
## and gets the inverse from the cache (if previously calculated). 
## If not available, it calculates the inverse and sets it in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve (data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
