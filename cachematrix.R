## below you will find 2 functions, makeCacheMatrix and cachesolve,
##  which cache the inverse of a given matrix

## makeCacheMatrix is a function, which creates a matrix, which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
      x <<- y
      i <<- NULL
    }
  get <- function() x
  setInverse <- function(solveMatrix) i <<- solveMatrix
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function retrieves the inverse of a matrix returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  
  if(!is.null(i)){
    message("getting cached data")
   
     return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i      
}
