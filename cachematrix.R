## the goal of these two functions is to avoid recalculation of inverse of a matrix already calculated,
## if this matrix has not changed

## The fisrt function create a matrix and its function to get and set it and to invert it
## This function is similar to makeVector but with a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  seti <- function(y) {
    x <<- y
    inv <<- NULL
  }
  geti <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(seti = seti, geti = geti,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function check if we have already the result of the inverse stored, returned it if yes
## calculate it if not store it and returned it

cacheSolve <- function(x, ...) {
  ## Assign to inv the result of getinverse, if first time used, inv=Null
  inv <- x$getinverse()
  ##Check if we have a stored value for the inverse 
    if(!is.null(inv)) {
    message("getting cached data")  ##message to prove the efficiency
    return(inv) ##return of the stored result and end of the function
  }
  data <- x$geti() ##matrix to invert assignmentt
  inv <- solve(data, ...) ##inverse calculation
  x$setinverse(inv) ## store result
  inv ##return result
}
