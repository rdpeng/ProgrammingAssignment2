## The following two functions help each other in caching a matrix's inverse
## The first one create a wrap-around environment for two objects of our concern:
## the matrix and its inverse. This environment includes other functions, which are
## capable of manipulating the matrix and its inverse by using the operator <<-
## these child functions are what the parent function returns.
## The second one look into the first one to decide whether or not should it
## calculate the new inverse or simply using the calculated one


## makeCacheMatrix(), as described above, returns a list of 4 functions:
## set(newx) allow user to replace the current matrix with a new one
## this function also reverse inv to back NULL accordingly
## get() return the current matrix
## setinverse(newinv) replace the current inverse matrix with newinv
## this function is recommended to be used only by cacheSolve()
## getinverse(newinv) return the current inversematrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(newx) {
    x <<- newx
    inverse <<- NULL
  }
  setinverse = function(newinv) {
    inverse <<- newinv
  }
  get <- function() x
  getinverse <- function() inverse
  list(set = set, setinverse = setinverse, get = get, getinverse = getinverse)
}


## This function first check whether or not x's inverse has been computed or not
## If it is, the function throw a message that it simply return the stored one,
## If not (this is because x have just been set to newx and inv is set to NULL),
## the function calculate the new inverse matrix and use setinverse to inject it
## into makeCacheMatrix()'s environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinverse()
  if (!is.null(inv)) {
    message("getting cached value")
    return (inv)
  }
  inv = solve(x$get(), ...)
  x$setinverse(inv) 
  inv
}
