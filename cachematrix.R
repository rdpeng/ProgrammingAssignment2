## A pair of functions that cache the inverse of a matrix.
##  This function creates a special "matrix" object that can cache its inverse.

## initialized inv as NULL; will hold value of matrix inverse
## value of matrix in parent environment
## if there is a new matrix, reset inv to NULL
## define the get fucntion - returns value of the matrix argument
## assigns value of inv in parent environment
##  ## gets the value of inv where called

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
get <- function() x
  setInverse <- function(solveMatrix) inv <<- solveMatrix
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

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
