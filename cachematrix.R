## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}


## The pair of functions is used in order to cache the inverse of a matrix.
## This function first sets a special "matrix" object to allow it to be cached for the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
## The functions here were to get the established matrix, set for matrix inverse, and get the inverse. 
  get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by the makeCacheMatrix above.
  cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
      message("getting cached data")
      return(inv)
}
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv      
}
