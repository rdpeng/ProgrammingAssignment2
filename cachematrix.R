## Initially,I simply set the input x as a matrix in the function
##inv is used to store the inverse of the matrix.
##and then set the solved value "inv" as a null
##then I found the inverse of matrix by using setInverse and getInverse vectors

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


## The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix.

cacheSolve  <- function(x, ...) {
        ##Getting the cache value 
  inv <- x$getInverse()
  #if a cached value exists ,return the value
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  #Otherwise get the matrix, calculate the inverse and store it in
  # the cache
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  #returning the inverse of the matrix.
  inv      
}
