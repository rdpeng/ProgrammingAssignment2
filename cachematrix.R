## Programs written for Rprogramming course
## The functions calculate the inverse of a matrix
## Caching is implemented to save computation time. That is
## the inverse of the matrix is stored in cache and each
## time the function to inverse matrix is called the cache is checked first.

## Write a short comment describing this function
## The makeCacheMatrix function will set the value of the matrix, get the value of matrix,
## set the value of the inverse matrix, and get the value of the inverse matrix.
## inv variable holds the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function(){
    x
  }
  ## solve function calculates the inverse of the matrix
  setInverse <- function(solve) { inv <<- solve(x)} 
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve function calculates the inverse of the matrix created in
## the MakeCacheMatrix function. It first checks if the inverse is in cache (ie. already
## calculated). If it is, then it uses that value and if it is not, it calculates the 
## inverse of the matrix.
cacheSolve <- function(x) {
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
