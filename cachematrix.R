## Put comments here that give an overall description of what your
## functions do

## 2 functions
## Function 1 makecacheMatrix will greate a list of functions to set the square matrix, get the square matrix
## set the inverse of the square matrix  or get the invers of the square matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                             ## initialize inv to Null
  set <- function(y) {                    ## Set the square matrix
    x <<- y
    inv <<- NULL                          ## new matrix will reinitialze null
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,              ## these lines create a list of 4 items for matrix functions
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()                   ## Retrieve inverse vale
  if(!is.null(inv)) {                     ## in not null then return inv value
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)      ## use solve function to calculate new inv value
  x$setinverse(inv)
  inv
}