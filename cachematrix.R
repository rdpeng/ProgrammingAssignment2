## a pair of functions that cache the inverse of a matrix.

## Creates a structure which allows for it to be checked
## if the matrix has been inverted already

makeCacheMatrix <- function(x = matrix()) {
  ## create NULL cache
  cache <- NULL
  ## create set function
  ## allows for resetting the matrix without
  ## calling on makeCacheMatrix again
  set <- function (matrix = matrix()){
    x <<- matrix
    cache <<- NULL
  }
  ## get function allows use to check the original
  ## matrix
  get <- function () x
  ## set.inverted takes a matrix and sets it as
  ## the cache (which is the inverted matrix)
  set.inverted <- function (matrix = matrix()){
    cache<<-matrix
  }
  ## get.inverted simply returns the inverted matrix
  ## or NULL if it hasn't been calculated yet
  get.inverted <- function (){
    return(cache)
  }
  ## this creates an object which has
  ## the 'set' name for the set() function
  ## 'get' for the get() function
  ## so on and so forth
  list(set = set, get = get, 
       set.inverted = set.inverted,
       get.inverted = get.inverted)
}


## Checks to see if a cache already exists for the
## inverted matrix, if it does, returns cache
## if it doesn't, calculate the inverted matrix,
## cache it, and return it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cache <- x$get.inverted()
  if(!is.null(cache)){
    message("getting cached data")
    return(cache)
  }
  else{
    inverted <- solve(x$get())
    x$set.inverted(inverted)
    x$get.inverted()
  }
}